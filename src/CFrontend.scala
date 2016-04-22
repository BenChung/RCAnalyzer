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

  val builtins = Map("PROTECT" -> Seq(LocalVarDecl("a",Ref)(NoPosition, NoInfo)),
    "allocVector" -> Seq(LocalVarDecl("a",Ref)(NoPosition, NoInfo), LocalVarDecl("b",Int)(NoPosition, NoInfo)))
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
      (iacc2, FuncApp(name, eargs)(getLocation(c), NoInfo, Ref, builtins(name)))
    }
  }


  def TranslateStatement(acc : Seq[Stmt], stmt : IASTStatement) : Seq[Stmt] = stmt match {
    case ds : CASTDeclarationStatement => ds.getDeclaration match {
      case sd : CASTSimpleDeclaration => acc ++ sd.getDeclarators.flatMap{
        decl =>{
          val temp = TempGen()
          val dtype = TranslateType(sd.getDeclSpecifier)
          val (niacc, inite) = decl.getInitializer match { case e : CASTEqualsInitializer => e.getInitializerClause match {
            case ei : IASTExpression => TranslateExpression(Seq(), ei)
          }}
          niacc.+:(LocalVarAssign(LocalVar(decl.getName.toString)(dtype, getLocation(sd), NoInfo), inite)(getLocation(stmt), NoInfo))
        }
      }
    }
  }

  def TranslateNode(node : IBasicBlock) : Block = node match {
    // plain nodes are our expressions.
    case b : CxxPlainNode => {
      val stmts = b.getData match {
        case stmt : IASTStatement => TranslateStatement(Seq(), stmt)
      }
      stmts.reverse.foldLeft(TranslateNode(b.getOutgoing)){ case (onode, nstmt) => NormalBlock(nstmt, onode)}
    }
    // exit nodes are our returns.
    case b : CxxExitNode => new TerminalBlock(Assert(TrueLit()(NoPosition,NoInfo))(NoPosition,NoInfo))
    case b : CxxStartNode => TranslateNode(b.getOutgoing)
  }

  def CFGTranslate(map:NodeCommentMap)(graph : CxxControlFlowGraph) = TranslateNode(graph.getStartNode)

  def main(args: Array[String]): Unit = {
    val file = FileContent.createForExternalFileLocation(args(0))
    val transunit = GCCLanguage.getDefault.getASTTranslationUnit(file,
      new ScannerInfo(),
      IncludeFileContentProvider.getEmptyFilesProvider,
      EmptyCIndex.INSTANCE,
      0,
      new StdoutLogService())

    val cmap = ASTCommenter.getCommentedNodeMap(transunit)
    val cfgs = transunit.getDeclarations
      .collect{ case e:IASTFunctionDefinition => e }
      .map{de => CxxControlFlowGraph.build(de)}

    val translated = cfgs.map(CFGTranslate(cmap))
    val f00 = 2+2
  }
}

class CFrontend {

}
