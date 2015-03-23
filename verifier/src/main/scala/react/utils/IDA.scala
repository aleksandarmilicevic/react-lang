package react.utils

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import java.io._

/** wrapper around the IDA solver from the sundials library */
class IDA(inputs: Seq[Variable], formula: Formula) {

  def keep(f: Formula) = f match {
    case Eq(_,_) => true
    case other =>
      Logger("KINSOL", Warning, "KINSOL supports only EQ for the moment, ignoring: " + other)
      false
  }

  val dtSuffix = "_dt"
  def dt(v: Variable): Variable = Variable(v.name + dtSuffix).setType(Real)
  def replaceDt(f: Formula): Formula = {
    FormulaUtils.map({
      case Application(DRealDecl.timeDerivative, List(v @ Variable(_))) => dt(v)
      case a @ Application(DRealDecl.timeDerivative, _) => sys.error("not normalized: " + a)
      case other => other
    }, f)
  }

  def getLHS(f: Formula): Formula = f match {
    case Eq(Literal(0.0) | Literal(0l), lhs) => lhs
    case Eq(lhs, Literal(0.0) | Literal(0l)) => lhs
    case Eq(lhs, rhs) => Minus(lhs, rhs)
    case other => Logger.logAndThrow("IDA", Error, "IDA supports only EQ for the moment, found: " + other)
  }

  lazy val conjuncts = FormulaUtils.getConjuncts(formula).map(getLHS).map(replaceDt).toIndexedSeq

  val ni = inputs.size
  lazy val nConj = conjuncts.size
  val varSeq = (formula.freeVariables -- inputs).toIndexedSeq
  val vars = varSeq.zipWithIndex.toMap
  val pvars = vars.map{ case (v,i) => dt(v) -> i }
  val nv = vars.size
  val nr = 0
  lazy val neq = if (nConj > nv) nConj + 1 else nConj

  def parameter = {
    "#define NI  " + ni  + "\n" + //nbr inputs
    "#define NEQ " + neq + "\n" + //nbr equations
    "#define NR  " + nr  + "\n" + //nbr roots
    "#define NV  " + nv  + "\n"   //nbr variables (if NEQ > NV then the difference is composed of dummy variables that should be 0)
  }

  def header = """/*
 * inputs: """ + inputs.mkString(", ") + """
 * variables: """ + varSeq.mkString(", ") + """
 *
 * residual function:
 *  """ + conjuncts.mkString("\n *  ") + """
 *
 * jacobian:
 *  """ + (for(i <- 0 until nConj; j <- 0 until nv) yield
            i + ", " + varSeq(j) + ": " + partialDif(i, j, Variable("cj"))
          ).mkString("\n *  ") + """
 */
"""
  
  def literal(d: Double) = "RCONST( "+d+" )"

  def access(v: Variable) = {
    if (inputs contains v) "((realtype*) user_data)[" + inputs.indexOf(v) + "]"
    else if (vars contains v) "yval[" + vars(v) + "]"
    else if (pvars contains v) "ypval[" + pvars(v) + "]"
    else if (v.name == "cj") "cj"
    else sys.error("unknown variable: " + v)
  }

  def residual(out: BufferedWriter) = {
    out.write("int residual(realtype tres, N_Vector yy, N_Vector yp, N_Vector rr, void *user_data) { ")
    out.newLine
    out.write("  realtype *yval = NV_DATA_S(yy);"); out.newLine
    out.write("  realtype *ypval = NV_DATA_S(yp);"); out.newLine
    out.write("  realtype *rval = NV_DATA_S(rr);"); out.newLine; out.newLine
    for (i <- 0 until nConj) {
      out.write("  rval["+i+"] = "+CPrinter.expr(conjuncts(i), literal, access)+" ;")
      out.newLine
    }
    //dummy variables
    if (neq > nConj) {
      assert(neq == nConj + 1)
      val expr = (nv until neq).map(i => "yval["+i+"]*yval["+i+"]").mkString(" + ")
      out.write("  rval["+nConj+"] = " + expr + ";")
      out.newLine
    }
    out.newLine
    out.write("  return 0;")
    out.newLine
    out.write("}")
    out.newLine
  }

  def printResidual(out: BufferedWriter) = {
    out.write("int printResidual(realtype tres, N_Vector yy, N_Vector yp, void *user_data) { ")
    out.newLine
    out.write("  realtype *yval = NV_DATA_S(yy);"); out.newLine
    out.write("  realtype *ypval = NV_DATA_S(yp);"); out.newLine
    for (i <- 0 until nConj) {
      out.write("  printf(\"rval["+i+"] = %10.4le\\n\", "+CPrinter.expr(conjuncts(i), literal, access)+");")
      out.newLine
    }
    //dummy variables
    if (neq > nConj) {
      assert(neq == nConj + 1)
      val expr = (nv until neq).map(i => "yval["+i+"]*yval["+i+"]").mkString(" + ")
      out.write("  printf(\"rval["+nConj+"] = %10.4le\\n\", "+expr+");")
      out.newLine
    }
    out.newLine
    out.write("  return 0;")
    out.newLine
    out.write("}")
    out.newLine
  }

  def root(out: BufferedWriter) = {
    out.write("int roots(realtype t, N_Vector yy, N_Vector yp, realtype *gout, void *user_data) { ")
    out.newLine
    // ???
    out.write("  return 0;")
    out.newLine
    out.write("}")
    out.newLine
  }

  def partialDif(i: Int, j: Int, cj: Variable): Formula = {
    val c = DRealDecl.timeDerivative(conjuncts(i))
    val v = varSeq(j)
    val d1 = ArithmeticSimplification.pushDerivativesDown(v, Set(), c)
    val d2 = ArithmeticSimplification.pushDerivativesDown(dt(v), Set(), c)
    val sum = Plus(d1, Times(cj, d2)) //dy + cj * dyp
    ArithmeticSimplification.polynomialNF(sum)
  }

  def jacobian(out: BufferedWriter) = {
    out.write("int jacobian(long int Neq, realtype tt, realtype cj,"); out.newLine
    out.write("             N_Vector yy, N_Vector yp, N_Vector rr,"); out.newLine
    out.write("             DlsMat JJ, void *user_data,"); out.newLine
    out.write("             N_Vector tempv1, N_Vector tempv2, N_Vector tempv3) {"); out.newLine
    out.write("  realtype *yval = NV_DATA_S(yy);"); out.newLine
    out.write("  realtype *ypval = NV_DATA_S(yp);"); out.newLine
    out.write("  realtype *rval = NV_DATA_S(rr);"); out.newLine
    out.newLine

    for(i <- 0 until nConj;
        j <- 0 until nv) {
      val jc = partialDif(i, j, Variable("cj"))
      val cexpr = CPrinter.expr(jc, literal, access)
      out.write("  DENSE_ELEM(JJ,"+i+","+j+") = "+cexpr+";")
      out.newLine
    }

    for(i <- nConj until neq - 1;
        j <- nv until neq) {
      out.write("  DENSE_ELEM(JJ,"+i+","+j+") = RCONST(0.0);")
      out.newLine
    }

    if (nConj < neq) {
      for(j <- nv until neq) {
        out.write("  DENSE_ELEM(JJ,"+nConj+","+j+") = 2 * yval["+j+"];")
        out.newLine
      }
    }

    out.write("  return 0;"); out.newLine
    out.write("}"); out.newLine
  }
  
  def jacTimesV(out: BufferedWriter) = {
    out.write("int jacTimesV(realtype tt, N_Vector yy, N_Vector yp, N_Vector rr,"); out.newLine
    out.write("              N_Vector v, N_Vector Jv, realtype cj,"); out.newLine
    out.write("              void *user_data, N_Vector tmp1, N_Vector tmp2) {"); out.newLine
    out.write("  realtype *yval = NV_DATA_S(yy);"); out.newLine
    out.write("  realtype *ypval = NV_DATA_S(yp);"); out.newLine
    out.write("  realtype *vval = NV_DATA_S(v);"); out.newLine
    out.write("  realtype *Jvval = NV_DATA_S(Jv);"); out.newLine
    out.write("  realtype tmp = RCONST( 0.0 );"); out.newLine
    //return J * v in Jv
    for(i <- 0 until nConj){
      out.write("  tmp = RCONST(0.0);"); out.newLine
      for(j <- 0 until nv) {
        val jc = partialDif(i, j, Variable("cj"))
        val cexpr = CPrinter.expr(jc, literal, access)
        out.write("  tmp += vval["+j+"] * ("+cexpr+");")
        out.newLine
      }
      out.write("  Jvval["+i+"] = tmp;"); out.newLine
    }
    if (nConj < neq) {
      out.write("  tmp = RCONST(0.0);"); out.newLine
      for(j <- nv until neq) {
        out.write("  tmp += vval["+j+"] * 2 * yval["+j+"];")
        out.newLine
      }
      out.write("  Jvval["+nConj+"] = tmp;"); out.newLine
    }
    out.write("  return 0;"); out.newLine
    out.write("}"); out.newLine
  }


  def psetup(out: BufferedWriter) = {
    out.write("int psetup(realtype tt, N_Vector yy, N_Vector yp, N_Vector rr,"); out.newLine
    out.write("           realtype cj, void *user_data,"); out.newLine
    out.write("           N_Vector tmp1, N_Vector tmp2, N_Vector tmp3) {"); out.newLine
    out.write("  return 0;"); out.newLine
    out.write("}"); out.newLine
  }

  def psolve(out: BufferedWriter) = {
    out.write("int psolve(realtype tt, N_Vector yy, N_Vector yp, N_Vector rr,"); out.newLine
    out.write("           N_Vector rvec, N_Vector zvec, realtype cj,"); out.newLine
    out.write("           realtype delta, void *user_data, N_Vector tmp) {"); out.newLine
    //solve the system J*z = r
    //or rather z = J^-1 * r
    ???
    out.write("  return 0;"); out.newLine
    out.write("}"); out.newLine
  }

  def makeFile: String = {
    //includes + parameter + residual + jacobian [+ roots] + errorChecking + printing + mainFunction
    val file = java.io.File.createTempFile("ida_react_model", ".c")
    val out = new BufferedWriter(new PrintWriter(file))
    out.write(header); out.newLine
    out.write(IDA.includes); out.newLine
    out.write(IDA.errorChecking); out.newLine
    out.write(parameter); out.newLine
    out.write(IDA.printing); out.newLine
    residual(out); out.newLine
    jacobian(out); out.newLine
    jacTimesV(out); out.newLine
    //psetup(out); out.newLine
    //psolve(out); out.newLine
    root(out); out.newLine
    printResidual(out); out.newLine
    out.write(IDA.mainFunction); out.newLine
    out.close
    file.getPath
  }

  def compile(fin: String) = {
    val fout = fin.replaceAll("\\.c", ".out")
    val cmd = Array("gcc", fin, "-o", fout, "-lsundials_ida", "-lsundials_nvecserial", "-lm", "-llapack", "-lblas", "-lquadmath")
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

  def solve(time: Double, inVal: Map[Variable, Double],
            initialValues: Map[Variable, Double],
            initialDt: Map[Variable, Double]): (Double, Map[Variable, Double], Map[Variable, Double]) = {
    if(!ready) prepare
    //time inVal initialValues initialDt
    val init = inputs.toArray.map(x => inVal(x).toString)
    val yy = varSeq.toArray.map(x => initialValues(x).toString)
    val yp = varSeq.toArray.map(x => initialDt.getOrElse(x, 0.0).toString)
    val cmd = Array(binary, time.toString) ++ init ++ yy ++ yp
    val (res, out, err) = SysCmd(cmd)
    if (res == 0) {
      val nums = out.split("\\s+").map(_.toDouble)
      var t = nums(0)
      val emp = Map[Variable, Double]()
      val (res, resDt) = vars.foldLeft( (emp,emp) )( (acc, v) => {
        val yres  = acc._1 + (v._1 -> nums(v._2+1) )
        val ypres = acc._2 + (v._1 -> nums(v._2+nv+1) )
        (yres, ypres)
      } )
      (t, res, resDt)
    } else {
      Logger.logAndThrow("IDA", Error, "IDA error("+res+"): " + cmd.mkString(" ") + "\n" + out + "\n" + err)
    }
  }

}

object IDA {

  protected def includes = """#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ida/ida.h>
#include <ida/ida_dense.h>
#include <ida/ida_spgmr.h>
#include <ida/ida_spbcgs.h>
#include <ida/ida_sptfqmr.h>
#include <nvector/nvector_serial.h>
#include <sundials/sundials_math.h>
#include <sundials/sundials_types.h>
"""

  val errorChecking = """
int check_flag(void *flagvalue, char *funcname, int opt)
{
  int *errflag;
  /* Check if SUNDIALS function returned NULL pointer - no memory allocated */
  if (opt == 0 && flagvalue == NULL) {
    fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed - returned NULL pointer\n\n", funcname);
    return 1;
  } else if (opt == 1) {
    /* Check if flag < 0 */
    errflag = (int *) flagvalue;
    if (*errflag < 0) {
      fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed with flag = %d\n\n", funcname, *errflag);
    return 1; 
    }
  }
  return 0;
}
"""

  val mainFunction = """
int main(int argc, char *argv[]) {
    int i = 0;
    void *solver;
    N_Vector yy, yp, avtol;
    realtype rtol, *yval, *ypval, *atval;
    realtype tout, tcur;
    int retval, retvalr;
    int rootsfound[NR];
    realtype user_data[NI];

    solver = NULL;
    yy = yp = avtol = NULL;
    yval = ypval = atval = NULL;

    /* Allocate N-vectors. */
    yy = N_VNew_Serial(NEQ);
    if(check_flag((void *)yy, "N_VNew_Serial", 0)) return(1);
    yp = N_VNew_Serial(NEQ);
    if(check_flag((void *)yp, "N_VNew_Serial", 0)) return(1);
    avtol = N_VNew_Serial(NEQ);
    if(check_flag((void *)avtol, "N_VNew_Serial", 0)) return(1);

    tcur = RCONST( 0.0 );
    tout = RCONST( atof(argv[1]) );
    
    for(i = 0; i < NI; i++) {
      user_data[i] = RCONST( atof(argv[ 2 + i ]) );
    }

    yval  = NV_DATA_S(yy);
    for(i = 0; i < NEQ; i++) {
      if (i < NV) {
        yval[i] = RCONST( atof(argv[ 2 + NI + i ]) );
      } else {
        yval[i] = RCONST( 0.0 );
      }
    }

    ypval = NV_DATA_S(yp);
    for(i = 0; i < NEQ; i++) {
      if (i < NV) {
        ypval[i] = RCONST( atof(argv[ 2 + NI + NV + i ]) );
      } else {
        ypval[i] = RCONST( 0.0 );
      }
    }

    rtol = RCONST(1.0e-4);
    atval = NV_DATA_S(avtol);
    for(i = 0; i < NEQ; i++) {
      atval[i] = RCONST(1.0e-6);
    }

    solver = IDACreate();
    if(check_flag((void *)solver, "IDACreate", 0)) return 1;

    retval = IDASetUserData(solver, user_data);
    if(check_flag(&retval, "IDASetUserData", 1)) return 1;

    retval = IDAInit(solver, residual, tcur, yy, yp);
    if(check_flag(&retval, "IDAInit", 1)) return 1;

    retval = IDASVtolerances(solver, rtol, avtol);
    if(check_flag(&retval, "IDASVtolerances", 1)) return 1;

    N_VDestroy_Serial(avtol);

    if (NR > 0) {
      retval = IDARootInit(solver, NR, roots);
      if (check_flag(&retval, "IDARootInit", 1)) return 1;
    }

    // Direct solver

    //retval = IDADense(solver, NEQ);
    //if(check_flag(&retval, "IDADense", 1)) return 1;
    //retval = IDALapackDense(solver, NEQ);
    //if(check_flag(&retval, "IDALapackDense", 1)) return 1;

    // jacobian for direct solver
    //retval = IDADlsSetDenseJacFn(solver, jacobian);
    //if(check_flag(&retval, "IDADlsSetDenseJacFn", 1)) return 1;


    // Krylov iteration solver
    retval = IDASpgmr(solver, 0);
    if(check_flag(&retval, "IDASpgmr", 1)) return(1);
    //retval = IDASpbcg(solver, 0);
    //if(check_flag(&retval, "IDASpbcg", 1)) return(1);
    //retval = IDASptfqmr(solver, 0);
    //if(check_flag(&retval, "IDASptfqmr", 1)) return(1);

    //retval = IDASpilsSetPreconditioner(solver, psetup, psolve)
    //if(check_flag(&retval, "IDASpilsSetPreconditioner", 1)) return(1);

    retval = IDASpilsSetJacTimesVecFn(solver, jacTimesV);
    if(check_flag(&retval, "IDASpilsSetJacTimesVecFn", 1)) return(1);



    //printResidual(tcur, yy, yp, user_data);

    //retval = IDACalcIC(solver, IDA_Y_INIT, tout);
    //if(check_flag(&retval, "IDACalcIC", 1)) return 1;
        
    //printResidual(tcur, yy, yp, user_data);

    while(tcur < tout) {

        retval = IDASolve(solver, tout, &tcur, yy, yp, IDA_NORMAL);

        //TODO
        //printResidual(tcur, yy, yp, user_data);

        if(check_flag(&retval, "IDASolve", 1)) return 1;

        if (retval == IDA_ROOT_RETURN) {
          retvalr = IDAGetRootInfo(solver, rootsfound);
          if(check_flag(&retvalr, "IDAGetRootInfo", 1)) return 1;
          //TODO
          //PrintRootInfo(rootsfound);
        }

        if (tcur >= tout) {
          if (retval == IDA_SUCCESS) {
            printResult(solver, tcur, yy, yp);
          } else {
            return -1;
          }
        }

    }

    //PrintFinalStats(solver);

    IDAFree(&solver);
    N_VDestroy_Serial(yy);
    N_VDestroy_Serial(yp);
    return 0;
}
"""

  val printing = """
void printResult(void* solver, realtype time, N_Vector yy, N_Vector yp) {
  int i = 0;
  realtype* yval  = NV_DATA_S(yy);
  realtype* ypval = NV_DATA_S(yp);

  #if defined(SUNDIALS_EXTENDED_PRECISION)
  printf("%10.4Le\n", time);
  for (i = 0; i < NV; i++) printf("%10.4Le\n", yval[i]);
  for (i = 0; i < NV; i++) printf("%10.4Le\n", ypval[i]);
  #elif defined(SUNDIALS_DOUBLE_PRECISION)
  printf("%10.4le\n", time);
  for (i = 0; i < NV; i++) printf("%10.4le\n", yval[i]);
  for (i = 0; i < NV; i++) printf("%10.4le\n", ypval[i]);
  #else
  printf("%10.4e\n", time);
  for (i = 0; i < NV; i++) printf("%10.4e\n", yval[i]);
  for (i = 0; i < NV; i++) printf("%10.4e\n", ypval[i]);
  #endif
}
"""

}
