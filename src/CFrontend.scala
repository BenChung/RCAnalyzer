/**
  * Created by Ben Chung on 4/17/2016.
  */

import org.eclipse.cdt.codan.core.cxx.internal.model.cfg._
import org.eclipse.cdt.codan.core.model.cfg._
import org.eclipse.cdt.codan.internal.core.cfg._
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier.IASTEnumerator
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind
import org.eclipse.cdt.core.dom.ast.cpp.{ICPPASTTemplateParameter, _}
import org.eclipse.cdt.core.dom.ast.{IASTAttributeSpecifier, IASTExpression, IASTTypeId, _}
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignator
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCompositeTypeSpecifier.ICPPASTBaseSpecifier
import org.eclipse.cdt.core.dom.ast.gnu.c._
import org.eclipse.cdt.core.parser.{FileContent, IncludeFileContentProvider, ScannerInfo}
import org.eclipse.cdt.internal.core.dom.parser.ASTAmbiguousNode
import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.internal.core.dom.rewrite.commenthandler.{ASTCommenter, NodeCommentMap}
import org.eclipse.cdt.internal.core.index.EmptyCIndex
import org.eclipse.cdt.internal.core.indexer.StdoutLogService
import viper.silver.ast._

object HelloWorld {

  def TempGen() : String = "temp" //TODO

  def TranslateType(d : IASTDeclSpecifier) = d match {
    case c : CASTTypedefNameSpecifier => Ref
  }

  private def getLocation(el : IASTNode) = {
    val loc = el.getFileLocation
    LineColumnPosition(loc.getStartingLineNumber, loc.getNodeOffset)
  }

  val builtins : Map[String,(Seq[LocalVarDecl], Type)] = Map(
    "PROTECT" -> (List(LocalVarDecl("a",Ref)(NoPosition, NoInfo)), Ref),
    "REAL" -> (Seq(LocalVarDecl("a",Ref)(NoPosition, NoInfo)), DomainType("IArray",Map(TypeVar("T") -> Ref))(Seq(TypeVar("T")))),
    "allocVector" -> (Seq(LocalVarDecl("a",Ref)(NoPosition, NoInfo), LocalVarDecl("b",Int)(NoPosition, NoInfo)), Ref),
    "asReal" -> (Seq(LocalVarDecl("a",Ref)(NoPosition, NoInfo)), Int),
    "UNPROTECT" -> (Seq(LocalVarDecl("a",Int)(NoPosition, NoInfo)),Int))

  def TranslateExpression(acc : Seq[Stmt], exp:IASTExpression) : (Seq[Stmt], Exp) = exp match {
    case n : CASTLiteralExpression => n.getExpressionType match {
      case cbt : CBasicType => (acc, cbt.getKind match {
        //case Kind.eChar =>
        //case Kind.eWChar
        case Kind.eInt => IntLit(BigInt(n.getValue.mkString))(getLocation(n), NoInfo)
        /*case Kind.eFloat
        case Kind.eDouble
        case Kind.eBoolean
        case Kind.eChar16
        case Kind.eChar32
        case Kind.eNullPtr
        case Kind.eInt128
        case Kind.eFloat128
        case Kind.eDecimal32
        case Kind.eDecimal64
        case Kind.eDecimal128*/
      })
    }
    case id : CASTIdExpression => (acc, LocalVar(id.getName.toString)(Ref, getLocation(id), NoInfo))
    case c : CASTFunctionCallExpression => {
      val (iacc2, eargs) = c.getArguments.reverse.foldLeft((acc,Seq[Exp]())){
        case ((iacc, args), iex : IASTExpression) => TranslateExpression(iacc, iex) match {
          case (nseq, nex) => (nseq, args.+:(nex))}}
      val name = c.getFunctionNameExpression match {
        case cide : CASTIdExpression => cide.getName.toString
      }
      val (args, rtype) = builtins(name)
      (iacc2, FuncApp(name, eargs)(getLocation(c), NoInfo, rtype, args))
    }
    case ase : CASTArraySubscriptExpression => {
      val (acc1, arr) = TranslateExpression(acc, ase.getArrayExpression)
      val (acc2, sub) = TranslateExpression(acc1, ase.getSubscriptExpression)
      (acc2,FieldAccess(FuncApp("loc", Seq(arr, sub))
      (NoPosition,NoInfo,Ref,
        Seq(LocalVarDecl("a", DomainType("IArray",Map(TypeVar("T") -> Ref))(Seq(TypeVar("T"))))(NoPosition,NoInfo),
          LocalVarDecl("b", Int)(NoPosition,NoInfo))),
        Field("val",Int)(NoPosition,NoInfo)) (getLocation(ase),NoInfo))
    }
    case bine : CASTBinaryExpression => {
      bine.getOperator match {
        case IASTBinaryExpression.op_plus => {
          val (acc1,lhs) = TranslateExpression(acc, bine.getOperand1)
          val (acc2,rhs) = TranslateExpression(acc1, bine.getOperand2)
          (acc2, Add(lhs, rhs)(getLocation(bine), NoInfo))
        }
      }
    }
  }


  def TranslateStatement(acc : Seq[Stmt], stmt : IASTStatement) : Seq[Stmt] = stmt match {
    case ds : CASTDeclarationStatement => ds.getDeclaration match {
      case sd : CASTSimpleDeclaration => acc ++ sd.getDeclarators.flatMap{
        decl =>{
          val dtype = TranslateType(sd.getDeclSpecifier)
          val (niacc, inite) = decl.getInitializer match { case e : CASTEqualsInitializer => e.getInitializerClause match {
            case ei : IASTExpression => TranslateExpression(Seq(), ei)
          }}
          niacc.+:(LocalVarAssign(LocalVar(decl.getName.toString)(dtype, getLocation(sd), NoInfo), inite)(getLocation(stmt), NoInfo))
        }
      }
    }
    case exps : CASTExpressionStatement => {
      exps.getExpression match {
        case bine : CASTBinaryExpression => {
          bine.getOperator match {
            case IASTBinaryExpression.op_assign => {
              val (acc1,lhs) = TranslateExpression(acc, bine.getOperand1)
              val (acc2,rhs) = TranslateExpression(acc1, bine.getOperand2)
              lhs match {
                case si : FieldAccess => acc2.+:(FieldAssign(si, rhs) (NoPosition,NoInfo))
                case v : LocalVar => acc2.+:(LocalVarAssign(v,rhs)(getLocation(bine),NoInfo))
              }
            }
          }
        }
        case fncall : CASTFunctionCallExpression => {
          val (acci, exp) = TranslateExpression(acc, fncall)
          acci.+:(LocalVarAssign(LocalVar(TempGen())(Int, getLocation(fncall), NoInfo), exp)(getLocation(fncall),NoInfo))
        }
      }
    }
  }

  def TranslateNode(node : IBasicBlock, map:NodeCommentMap, outName :String) : Block = node match {
    // plain nodes are our expressions.
    case b : CxxPlainNode => {
      val stmts = b.getData match {
        case stmt : IASTStatement => TranslateStatement(Seq(), stmt)
      }
      NormalBlock(Seqn(stmts)(NoPosition, NoInfo), TranslateNode(b.getOutgoing, map, outName))
    }
    // exit nodes are our returns.
    case b : CxxExitNode =>
      b.getData match {
        case null => new TerminalBlock(Assert(TrueLit()(NoPosition,NoInfo))(NoPosition,NoInfo))
        case e : CASTReturnStatement => {
          val (acc, texp) = TranslateExpression(Seq(), e.getReturnValue)
          val last = TerminalBlock(LocalVarAssign(LocalVar(outName)(Ref, NoPosition, NoInfo), texp)(getLocation(e), NoInfo))
          acc match {
            case Seq() => last
            case _ => NormalBlock(Seqn(acc)(NoPosition, NoInfo), last)
          }
        }
      }
    case b : CxxStartNode => TranslateNode(b.getOutgoing, map, outName)
  }

  def CFGTranslate(map:NodeCommentMap)(graph : CxxControlFlowGraph) = TranslateNode(graph.getStartNode, map, "foobar")

  def main(args: Array[String]): Unit = {
    val file = FileContent.createForExternalFileLocation(args(0))
    val transunit = GCCLanguage.getDefault.getASTTranslationUnit(file,
      new ScannerInfo(),
      IncludeFileContentProvider.getEmptyFilesProvider,
      EmptyCIndex.INSTANCE,
      0,
      new StdoutLogService())

    val cmap = ASTCommenter.getCommentedNodeMap(transunit)
    val translated = transunit.getDeclarations
      .collect{ case e:IASTFunctionDefinition => {
        val argnames = e.getDeclarator match {
          case c : CASTFunctionDeclarator => c.getParameters.map{pm => pm.getDeclSpecifier match {
            case ctdns : CASTTypedefNameSpecifier => LocalVarDecl(pm.getDeclarator.getName.toString, Ref)(NoPosition, NoInfo)
          }}
        }
        val fnname = e.getDeclarator match {
           case fdn : CASTFunctionDeclarator =>fdn.getName.toString
         }
        val body = CFGTranslate(cmap)(CxxControlFlowGraph.build(e))
        Method(fnname, argnames, Seq(LocalVarDecl("foobar", Ref)(NoPosition,NoInfo)), Seq(), Seq(), Seq(), body.toAst)(NoPosition, NoInfo)
      }}

    val f00 = 2+2
  }
}

class CFrontend {

}
