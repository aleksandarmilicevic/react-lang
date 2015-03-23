package react.utils

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import java.io._

/** wrapper around the KINSOL solver from the sundials library */
class KINSOL(inputs: IndexedSeq[Variable], formula: Formula) {

  def keep(f: Formula) = f match {
    case Eq(_,_) => true
    case other =>
      Logger("KINSOL", Warning, "KINSOL supports only EQ for the moment, ignoring: " + other)
      false
  }

  def getLHS(f: Formula): Formula = f match {
    case Eq(Literal(0.0) | Literal(0l), lhs) => lhs
    case Eq(lhs, Literal(0.0) | Literal(0l)) => lhs
    case Eq(lhs, rhs) => Minus(lhs, rhs)
    case other => Logger.logAndThrow("KINSOL", Error, "KINSOL supports only EQ for the moment, found: " + other)
  }

  lazy val conjuncts = FormulaUtils.getConjuncts(formula).filter(keep).map(getLHS).toIndexedSeq

  val ni = inputs.size
  lazy val neq = conjuncts.size
  val varSeq = (formula.freeVariables -- inputs).toIndexedSeq
  val vars = varSeq.zipWithIndex.toMap
  val nv = vars.size

  def parameter = {
    "#define NI  " + ni  + "\n" + //nbr inputs
    "#define NV  " + nv  + "\n" + //nbr variables 
    "#define NEQ " + neq + "\n"   //nbr equations
  }
  
  def header = """/*
 * inputs: """ + inputs.mkString(", ") + """
 * variables: """ + varSeq.mkString(", ") + """
 *
 * residual function:
 *  """ + conjuncts.mkString("\n *  ") + """
 *
 * jacobian:
 *  """ + (for(i <- 0 until neq; j <- 0 until nv) yield
            i + ", " + varSeq(j) + ": " + partialDif(i, j)
          ).mkString("\n *  ") + """
 */
"""

  def literal(d: Double) = "RCONST( "+d+" )"

  def access(v: Variable) = {
    if (inputs contains v) "((realtype*) user_data)[" + inputs.indexOf(v) + "]"
    else if (vars contains v) "yval[" + vars(v) + "]"
    else sys.error("unknown variable: " + v)
  }

  def residual(out: BufferedWriter) = {
    out.write("int residual(N_Vector yy, N_Vector rr, void *user_data) { "); out.newLine
    out.write("  realtype *yval = NV_DATA_S(yy);"); out.newLine
    out.write("  realtype *rval = NV_DATA_S(rr);"); out.newLine; out.newLine
    for (i <- 0 until neq) {
      out.write("  rval["+i+"] = "+CPrinter.expr(conjuncts(i), literal, access)+" ;")
      out.newLine
    }
    out.newLine
    out.write("  return 0;"); out.newLine
    out.write("}"); out.newLine
  }
  
  def printResidual(out: BufferedWriter) = {
    out.write("int printResidual(N_Vector yy, void *user_data) { "); out.newLine
    out.write("  realtype *yval = NV_DATA_S(yy);"); out.newLine
    for (i <- 0 until neq) {
      out.write("  printf(\"rval["+i+"] = %10.4le\\n\", "+CPrinter.expr(conjuncts(i), literal, access)+");")
      out.newLine
    }
    out.newLine
    out.write("  return 0;"); out.newLine
    out.write("}"); out.newLine
  }

  def partialDif(i: Int, j: Int): Formula = {
    val c = DRealDecl.timeDerivative(conjuncts(i))
    val v = varSeq(j)
    val d = ArithmeticSimplification.pushDerivativesDown(v, Set(), c)
    ArithmeticSimplification.polynomialNF(d)
  }
  
  def jacobian(out: BufferedWriter) = {
    out.write("int jacobian(long int neq, N_Vector yy, N_Vector rr,"); out.newLine
    out.write("             DlsMat JJ, void *user_data,"); out.newLine
    out.write("             N_Vector tempv1, N_Vector tempv2) {"); out.newLine
    out.write("  realtype *yval = NV_DATA_S(yy);"); out.newLine
    out.write("  realtype *rval = NV_DATA_S(rr);"); out.newLine
    out.newLine

    for(i <- 0 until neq;
        j <- 0 until nv) {
      val jc = partialDif(i, j)
      val cexpr = CPrinter.expr(jc, literal, access)
      out.write("  DENSE_ELEM(JJ,"+i+","+j+") = "+cexpr+";")
      out.newLine
    }

    out.write("  return 0;"); out.newLine
    out.write("}"); out.newLine
  }

  def makeFile: String = {
    //includes + parameter + residual + jacobian [+ roots] + errorChecking + printing + mainFunction
    val file = java.io.File.createTempFile("kinsol_react_model", ".c")
    val out = new BufferedWriter(new PrintWriter(file))
    out.write(header); out.newLine
    out.write(KINSOL.includes); out.newLine
    out.write(KINSOL.errorChecking); out.newLine
    out.write(parameter); out.newLine
    out.write(KINSOL.printing); out.newLine
    residual(out); out.newLine
    jacobian(out); out.newLine
    //jacTimesV(out); out.newLine
    //psetup(out); out.newLine
    //psolve(out); out.newLine
    printResidual(out); out.newLine
    out.write(KINSOL.mainFunction); out.newLine
    out.close
    file.getPath
  }

  def compile(fin: String) = {
    val fout = fin.replaceAll("\\.c", ".out")
    val cmd = Array("gcc", fin, "-o", fout, "-lsundials_kinsol", "-lsundials_nvecserial", "-lm", "-llapack", "-lblas", "-lquadmath")
    val (res, out, err) = SysCmd(cmd)
    if (res == 0) {
      fout
    } else {
      Logger.logAndThrow("IDA", Error, "compilation error: " + cmd.mkString(" ") + "\n" + out + "\n" + err)
    }
  }

  protected var ready = false
  protected var source = ""
  protected var binary = ""

  def clean {
    ready = false
    if (source != "") new File(source).delete()
    if (binary != "") new File(binary).delete()
  }
    
  def prepare {
    source = makeFile
    binary = compile(source)
    ready = true
  }
  
  def solve(inVal: Map[Variable, Double],
            initialValues: Map[Variable, Double]): Map[Variable, Double] = {
    if (!ready) prepare
    //time inVal initialValues initialDt
    val init = inputs.toArray.map(x => inVal(x).toString)
    val yy = varSeq.toArray.map(x => initialValues.getOrElse(x, 1.0).toString)
    val cmd = Array(binary) ++ init ++ yy
    val (res, out, err) = SysCmd(cmd)
    if (res == 0) {
      val nums = out.split("\\s+").map(_.toDouble)
      var t = nums(0)
      val emp = Map[Variable, Double]()
      vars.foldLeft( emp )( (acc, v) => {
        acc + (v._1 -> nums(v._2) )
      } )
    } else {
      Logger.logAndThrow("KINSOL", Error, "KINSOL error("+res+"): " + cmd.mkString(" ") + "\n" + out + "\n" + err)
    }
  }

}

object KINSOL {

  protected def includes = """#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <kinsol/kinsol.h>
#include <kinsol/kinsol_dense.h>
#include <kinsol/kinsol_spgmr.h>
#include <kinsol/kinsol_spbcgs.h>
#include <kinsol/kinsol_sptfqmr.h>
#include <nvector/nvector_serial.h>
#include <sundials/sundials_math.h>
#include <sundials/sundials_types.h>
"""
  val errorChecking = IDA.errorChecking

  val mainFunction = """
int main(int argc, char *argv[]) {
  int i = 0;
  void *solver;
  N_Vector yy, scale, constraints;
  realtype ftol, stol;
  int retval;
  realtype user_data[NI];

  solver = NULL;
  yy = scale = constraints = NULL;

  ftol = RCONST(1.e-5); /* function tolerance */
  stol = RCONST(1.e-5); /* step tolerance */
    
  for(i = 0; i < NI; i++) {
    user_data[i] = RCONST( atof(argv[ 1 + i ]) );
  }

  yy = N_VNew_Serial(NEQ);
  if(check_flag((void *)yy, "N_VNew_Serial", 0)) return(1);
  scale = N_VNew_Serial(NEQ);
  if (check_flag((void *)scale, "N_VNew_Serial", 0)) return(1);
  constraints = N_VNew_Serial(NEQ);
  if (check_flag((void *)constraints, "N_VNew_Serial", 0)) return(1);

  solver = KINCreate();
  if (check_flag((void *)solver, "KINCreate", 0)) return(1);
    
  retval = KINSetUserData(solver, user_data);
  if(check_flag(&retval, "KINSetUserData", 1)) return 1;

  retval = KINInit(solver, residual, yy); /* y passed as a template */
  if (check_flag(&retval, "KINInit", 1)) return(1);

  /* Set optional inputs */

  N_VConst_Serial(RCONST(0.0),constraints);
  for (i = 0; i < NEQ; i++) NV_Ith_S(constraints, i) = RCONST(0.0);
  retval = KINSetConstraints(solver, constraints);
  if (check_flag(&retval, "KINSetConstraints", 1)) return(1);
  retval = KINSetFuncNormTol(solver, ftol);
  if (check_flag(&retval, "KINSetFuncNormTol", 1)) return(1);
  retval = KINSetScaledStepTol(solver, stol);
  if (check_flag(&retval, "KINSetScaledStepTol", 1)) return(1);

  /* Attach dense linear solver */
  retval = KINDense(solver, NEQ);
  if (check_flag(&retval, "KINDense", 1)) return(1);
  retval = KINDlsSetDenseJacFn(solver, jacobian);
  if (check_flag(&retval, "KINDlsSetDenseJacFn", 1)) return(1);
  retval = KINSetMaxSetupCalls(solver, 1);
  if (check_flag(&retval, "KINSetMaxSetupCalls", 1)) return(1);

//retval = KINSpgmr(solver, 15);
//if (check_flag(&retval, "KINSpgmr", 1)) return(1);
//retval = KINSpilsSetMaxRestarts(solver, 2);
//if (check_flag(&retval, "KINSpilsSetMaxRestarts", 1)) return(1);

  /* Initial guess */
  N_VConst_Serial(RCONST(0.0), yy);
  for(i = 0; i < NV; i++) {
    int idx = 1+NI+i;
    if (idx < argc)
      NV_Ith_S(yy,i) = RCONST( atof(argv[idx]) );
  }
  
  //printResidual(yy, user_data);

  /* solving */
  N_VConst_Serial(RCONST(1.0),scale);
  retval = KINSol(solver, yy,
                  KIN_LINESEARCH, /* global stragegy choice */
                  scale,          /* scaling vector, for the variable cc */
                  scale);         /* scaling vector for function values fval */
  if (check_flag(&retval, "KINSol", 1)) return(1);

  printResult(solver, yy);

  N_VDestroy_Serial(yy);
  N_VDestroy_Serial(scale);
  N_VDestroy_Serial(constraints);
  KINFree(&solver);
  return 0;
}
"""
  
  val printing = """
void printResult(void* solver, N_Vector yy) {
  int i = 0;
  realtype* yval  = NV_DATA_S(yy);

  #if defined(SUNDIALS_EXTENDED_PRECISION)
  for (i = 0; i < NV; i++) printf("%10.4Le\n", yval[i]);
  #elif defined(SUNDIALS_DOUBLE_PRECISION)
  for (i = 0; i < NV; i++) printf("%10.4le\n", yval[i]);
  #else
  for (i = 0; i < NV; i++) printf("%10.4e\n", yval[i]);
  #endif
}
"""

}
