package react.utils

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import java.io._

/** wrapper around the IDA solver from the sundials library */
class IDA(inputs: Seq[Variable], formula: Formula) {

  //TODO inputs

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

  val conjuncts = FormulaUtils.getConjuncts(formula).map(getLHS).map(replaceDt).toIndexedSeq

  val ni = inputs.size
  val nConj = conjuncts.size
  val varSeq = (formula.freeVariables -- inputs).toIndexedSeq
  val vars = varSeq.zipWithIndex.toMap
  val pvars = vars.map{ case (v,i) => dt(v) -> i }
  val nv = vars.size
  val nr = 0
  val neq = if (nConj > nv) nConj + 1 else nConj

  def parameter = {
    "#define NI  " + ni  + "\n" + //nbr inputs
    "#define NEQ " + neq + "\n" + //nbr equations
    "#define NR  " + nr  + "\n" + //nbr roots
    "#define NV  " + nv  + "\n"   //nbr variables (if NEQ > NV then the difference is composed of dummy variables that should be 0)
  }
  
  def literal(d: Double) = "RCONST( "+d+" )"

  def access(v: Variable) = {
    if (inputs contains v) "((realtype*) user_data)[" + inputs.indexOf(v) + "]"
    if (vars contains v) "yval[" + vars(v) + "]"
    else if (pvars contains v) "ypval[" + pvars(v) + "]"
    else if (v.name == "cj") "cj"
    else sys.error("unknown variable: " + v)
  }

  def residual(out: BufferedWriter) = {
    out.write("int residual(realtype tres, N_Vector yy, N_Vector yp, N_Vector resval, void *user_data) { ")
    out.newLine; out.newLine
    out.write("  realtype *yval = NV_DATA_S(yy);")
    out.newLine
    out.write("  realtype *ypval = NV_DATA_S(yp);")
    out.newLine
    out.write("  realtype *rval = NV_DATA_S(rr);")
    out.newLine; out.newLine
    for (i <- 0 until nConj) {
      out.write("  rval["+i+"] = "+CPrinter.expr(conjuncts(i), literal, access)+" ;")
      out.newLine
    }
    if (neq > nConj) {
      assert(neq == nConj + 1)
      val expr = (nConj until neq).map(i => "yval["+i+"]").mkString(" + ")
      out.write("  rval["+nConj+"] = " + expr + ";")
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
      val c = DRealDecl.timeDerivative(conjuncts(i))
      val v = varSeq(j)
      val d1 = ArithmeticSimplification.pushDerivativesDown(v, Set(), c)
      val d2 = ArithmeticSimplification.pushDerivativesDown(dt(v), Set(), c)
      //println("c = " + c)
      //println("v = " + v)
      //println("d1 = " + d1)
      //println("d2 = " + d2)
      val sum = Plus(d1, Times(Variable("cj"), d2)) //dy + cj * dyp
      val jc = ArithmeticSimplification.polynomialNF(sum)
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
      for(j <- 0 until nv) {
        out.write("  DENSE_ELEM(JJ,"+nConj+","+j+") = RCONST(0.0);")
        out.newLine
      }
      for(j <- nv until neq) {
        out.write("  DENSE_ELEM(JJ,"+nConj+","+j+") = cj;")
        out.newLine
      }
    }

    out.write("  return 0;")
    out.newLine
    out.write("}")
    out.newLine
  }

  def makeFile: String = {
    //includes + parameter + residual + jacobian [+ roots] + errorChecking + printing + mainFunction
    val file = java.io.File.createTempFile("ida_react_model", ".c")
    val out = new BufferedWriter(new PrintWriter(file))
    out.write(IDA.includes); out.newLine
    out.write(IDA.errorChecking); out.newLine
    out.write(IDA.printing); out.newLine
    out.write(parameter); out.newLine
    residual(out); out.newLine
    jacobian(out); out.newLine
    root(out); out.newLine
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

  var source = ""
  var binary = ""

  def clean {
    if (source != "") new File(source).delete()
    if (binary != "") new File(binary).delete()
  }
    
  def prepare {
    source = makeFile
    binary = compile(source)
  }

  def solve(time: Double, inVal: Map[Variable, Double],
            initialValues: Map[Variable, Double],
            initialDt: Map[Variable, Double]): Map[Variable, Double] = {
    //time inVal initialValues initialDt
    ???
  }

}

object IDA {

  protected def includes = """#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ida/ida.h>
#include <ida/ida_dense.h>
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
    tout = RCONST( atof(argv[0]) );
    
    for(i = 0; i < NI; i++) {
      user_data[i] = RCONST( atof(argv[ 1 + i ]) );
    }

    yval  = NV_DATA_S(yy);
    for(i = 0; i < NEQ; i++) {
      if (i < NV) {
        yval[i] = RCONST( atof(argv[ 1 + NI + i ]) );
      } else {
        yval[i] = RCONST( 0.0 );
      }
    }

    ypval = NV_DATA_S(yp);
    for(i = 0; i < NEQ; i++) {
      if (i < NV) {
        ypval[i] = RCONST( atof(argv[ 1 + NI + NV + i ]) );
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

    retval = IDADense(solver, NEQ);
    if(check_flag(&retval, "IDADense", 1)) return 1;

    retval = IDADlsSetDenseJacFn(solver, jacobian);
    if(check_flag(&retval, "IDADlsSetDenseJacFn", 1)) return 1;

    while(tcur < tout) {

        retval = IDASolve(solver, tout, &tcur, yy, yp, IDA_NORMAL);

        //TODO
        //PrintOutput(solver,tcur,yy);

        if(check_flag(&retval, "IDASolve", 1)) return 1;

        if (retval == IDA_ROOT_RETURN) {
          retvalr = IDAGetRootInfo(solver, rootsfound);
          if(check_flag(&retvalr, "IDAGetRootInfo", 1)) return 1;
          //TODO
          //PrintRootInfo(rootsfound);
        }

        if (retval == IDA_SUCCESS) {
          tout *= RCONST(10.0);
        }

    }

    printResult(solver, tcur, yy, yp);
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
