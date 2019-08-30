#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]

using namespace Rcpp;
using namespace arma;
using namespace std;

// // [[Rcpp::export]]
// arma::mat project_pdc(const arma::mat &X, const double delta = 1e-6)
//   {
//   /*
//   * Projection onto positive definite cone
//   */
//   if (X.n_rows != X.n_cols) {
//     std::runtime_error("Not a square matrix");
//   }
//   arma::vec eigval;
//   arma::mat eigvec;
//   eig_sym(eigval, eigvec, X);
//   eigval.elem(find(eigval <= 0)).fill(delta);
//   return eigvec.st()*diagmat(eigval)*eigvec;
// }

// // [[Rcpp::export]]
arma::mat ppmat(const arma::mat &X) {
  /*
  * Positive part of matrix. Given matrix x, is positive part
  * is returned.
  */
  if (X.max() > 0) {
    return clamp(X,0,X.max());
  }
  return zeros<mat>(X.n_rows, X.n_cols);
}

// // [[Rcpp::export]]
arma::mat st(const arma::mat &X, const double &lam) {
  /*
  * Soft threshold of a matrix X with parameter lam.
  */
  return ppmat(X-lam) - ppmat(-X-lam);
}

arma::mat st_offdiag(const arma::mat &X, const double &lam) {
  /*
  * Soft threshold of a matrix X with parameter lam.
  * Diagonal is ignored in this version.
  */
  arma::mat Xdiag = diagmat(X);
  arma::mat Xoffdiag = X - Xdiag;
  Xoffdiag = ppmat(Xoffdiag-lam) - ppmat(-Xoffdiag-lam);
  return Xdiag + Xoffdiag;
}

// // [[Rcpp::export]]
arma::mat htHelper(arma::mat X, const int &s) {
  /*
  * Support set matrix. Given matrix X and integer 0 <= s <= |X|
  * returns a matrix of size X such that the top s absolute value
  * entries of X are returned in their original positions and the
  * rest are set to zero.
  */

  int N = X.n_rows*X.n_cols;

  // Ensure 0 <= s <= N;
  if (s < 0) {
    // Rcpp::Rcout << "case of s2 = " << s << " < 0" << endl;
    throw std::range_error("Sparsity parameter cannot be less than zero.");
  }
  if (s > N) {
    // Rcpp::Rcout << "case of s2 = " << s << " > N = " << N << endl;
    throw std::range_error("Sparsity parameter cannot larger than the number of matrix entries.");
  }
  if (s == 0) {
    X.zeros();
  }
  else if (s < N) {

    int p = N - s;                      // p is # of elements to set to 0
    arma::mat absX = abs(X);
    vec x = sort(vectorise(absX));      // x = sorted vectorized abs(X)
    X.elem(find(absX < x(p))).zeros();  // set lowest p elements in absolute value to zero
    // Above, find elements of abs(X) <=
    // value of x at position p - 1 &
    // set them to zero. The
    // result is the support set
    // matrix of X.
  }
  else {
    // The remaining logical case s == N returns the input matrix.
  }
  return X;
}

// [[Rcpp::export]]
arma::mat ht(arma::mat X, int s, bool ss = false) {
  /*
  * Hard threshold operator. Given a matrix, sparsity paramter s
  * (0 <= s <= |X|), and bool ss indicating whether or not X
  * is square symmetric.
  */
  if (s == 0) return X;
  if (not ss) X =  htHelper(X,s);
  else {
    // Otherwise, for a (square) symmetric matrix, the symmetry can be
    // exploited for speed.
    int n = X.n_rows;
    int s2 = (s-n+1)/2;
    arma::mat Xdiag = diagmat(X);
    arma::mat Xupper = htHelper(trimatu(X,1),s2);
    X = Xdiag + Xupper + trans(Xupper);
  }
  return X;
}

// // [[Rcpp::export]]
arma::mat minOmega(const arma::mat &B, const arma::mat &X, const arma::mat &Y,
                   const double &del = 1e-3) {
  /*
  * Direct minimization of tsmvrRcpp objective function with respect to
  * covariance matrix Omega.
  *
  * Given regression covariane matrix SigmaR (q-by-q), returns the
  * value of the (response) covariance matrix Sigma that minimizes
  * the tsmvrRcpp objective function when B is held constant (q-by-q
  * matrix.)
  *
  *       minOmega = [SigmaR]^(-1)
  */
  arma::mat A = Y-X*B;
  arma::mat Covariance = A.st()*A/X.n_rows;
  arma::mat temp = inv(Covariance+del*arma::eye<mat>(size(Covariance)));
  return temp;
}

// // [[Rcpp::export]]
arma::mat initialize_B(const arma::mat &S, const arma::mat &H, const int &s,
                       const double &lam = 0.1, const double &del = 1e-3) {
  /*
  * This function intializes in the initial iterates B
  * for iterative hard threholding for multivariate
  * regression. It takes design matrix p-by-p predictor matrix
  * S = X'X, p-by-q matrix H = X'Y, sparsity parameter integer
  * s >= 0, and Lasso paramter double lam > 0.
  */
  arma::mat B_init = ht(st(solve(S+del*arma::eye<mat>(size(S)),H,solve_opts::fast),lam),s);
  return B_init;
}

// // [[Rcpp::export]]
arma::mat initialize_Omega(const arma::mat &B0, const arma::mat &X, const arma::mat &Y,
                           const int &s, const double &lam = 0.1,
                           const double &del = 1e-3) {
  /*
  * This function intializes in the initial iterates B
  * for iterative hard threholding for multivariate
  * regression. It takes p-by-q initial iterate regrssor matrix B0,
  * design matrix n-by-p predictor matrix X, n-by-q matrix Y,
  * sparsity parameter integer s >= 0, and Lasso paramter double
  * lam > 0.
  */
  arma::mat minOm = minOmega(B0,X,Y,del);
  arma::mat Soft = st(minOm,lam);
  arma::mat Omega_init = ht(Soft,s,true);
  return Omega_init;
}

// // [[Rcpp::export]]
double objective(const arma::mat &B, const arma::mat &Omega, const arma::mat &X,
                 const arma::mat &Y)
{
  /*
  * Covariance estimated multivariate regression objective function
  * (i.e., log-likelihood function).
  *
  * Given regressor matrix B (p-by-q), PSD covariance matrix Omega
  * (q-by-n), design matrix X (p-by-n), and response matrix Y
  * (q-by-n), returns the value of objective function (double).
  *
  *  objective = 1/n Trace[ (Y-X*B)'*Omega*(Y-X*B) ] - log(|Omega|)
  */
  arma::mat A = Y-X*B;
  return trace(A*Omega*A.st())/X.n_rows - real(log_det(Omega));
}

// // [[Rcpp::export]]
arma::mat gdB(const arma::mat &B, const arma::mat &Omega,
              const arma::mat &S, const arma::mat &H,
              const int &n, const double &eta)
{
  /*
  * Gradient step with respect to B
  *
  * Given regressor matrix B (p-by-q), covariance matrix Omega (q-by-
  * n), and matrices S = X'X (p-by-p) and H = Y'X (q-by-q), number
  * of observations n, and learning rate eta, returns the gradient
  * descent step of B, B_new (a p-by-matrix)
  *
  *       B_new = B - eta*dgdB(B,Omega,S,H,n)
  */
  // const arma::mat dgdB = 2.0/n*(S*B-H)*Omega;
  // return (B - eta*dgdB(B,Omega,S,H,n));
  return (B - 2.0*eta*(S*B-H)*Omega/n);
}

// // [[Rcpp::export]]
arma::mat gdOmega(const arma::mat &B, const arma::mat &Omega,
                  const arma::mat &X, const arma::mat &Y,
                  const double &eta, const bool &disp_min_ev,
                  const double &del)
{
  /*
  * Gradient step with respect to Omega.
  *
  * Given regressor matrix B (p-by-q), covariance matrix Omega (q-by-
  * n), matrices X (n-by-p) and Y (q-by-n), and learning rate eta,
  * returns the gradient descent step of Omega,
  * Omega_new (a p-by-matrix).
  *
  *       Omega_new = Omega - eta*dgdB(B,Omega,X,Y)
  */
  if (disp_min_ev) {
    arma::vec eigval = eig_sym(Omega);
    Rcpp::Rcout << "Omega minimum eigenvalue = " << eigval[0] << endl;
  }
  const arma::mat A = Y-X*B;
  const arma::mat dgdOm = A.st()*A/X.n_rows -
     inv_sympd(Omega + del*arma::eye<mat>(size(Omega)));
  return (Omega - eta*dgdOm);
}

arma::mat lsB(const arma::mat &B, const arma::mat &Omega,
              const arma::mat &S, const arma::mat &H,
              const arma::mat &X, const arma::mat &Y,
              const int &s, double eta = 0.1,
              const double &rho = 1e2, const double &beta = 0.5,
              const double &qmax = 128)
{
  /*
  * Generalized linesearch for with resepct to B.
  *
  */
  arma::mat B_test;
  arma::mat B_diff;
  double g_test;
  double g;
  double g_diff;
  int q = -1;
  do {
    q++;
    eta = eta*pow(beta,q);
    // Rcpp::Rcout << "eta_1  = " << eta << endl;
    if (q > qmax) {
      throw std::runtime_error("B-step linesearch had too many iterations. Try adjusting parameters.");
    }
    B_test = ht(gdB(B,Omega,S,H,X.n_rows,eta),s);
    B_diff = B_test - B;
    g_test = objective(B_test,Omega,X,Y);
    g = objective(B,Omega,X,Y);
    g_diff = - rho/2*trace(B_diff.t()*B_diff);
  } while ( g_test > g + g_diff );
  return(B_test);
}

arma::mat lsOmega(arma::mat B, const arma::mat &Omega,
                  const arma::mat &X, const arma::mat &Y,
                  const int &s, double eta = 0.1,
                  const double &rho = 1e-6, const double &beta = 0.5,
                  const bool &disp_min_ev = false, const double &del = 1e-6,
                  const int &qmax = 128)
{
  /*
  * Generalized linesearch for with resepct to Omega.
  */
  arma::mat Omega_test;
  arma::mat Omega_diff;
  double g_test;
  double g;
  double g_diff;
  int q = -1;
  do {
    q++;
    eta = eta*pow(beta,q);
    if (q > qmax) {
      throw std::runtime_error("B-step linesearch had too many iterations. Try adjusting parameters.");
    }
    Omega_test = ht(gdOmega(B,Omega,X,Y,eta,disp_min_ev,del),s,true); // non psd step

    Omega_diff = Omega_test - Omega;
    g_test = objective(B,Omega_test,X,Y);
    g = objective(B,Omega,X,Y);
    g_diff = rho/2*trace(Omega_diff.t()*Omega_diff);
  } while (
      objective(B,Omega_test,X,Y) >
    objective(B,Omega,X,Y) -
    rho/2*trace(Omega_diff.t()*Omega_diff)
  );
  return(Omega_test);
}

// // [[Rcpp::export]]
arma::mat updateB(arma::mat B, const arma::mat &Omega,
                  const arma::mat &X, const arma::mat &Y,
                  const arma::mat &S, const arma::mat &H,
                  const std::string &type,
                  const int &s,
                  const double &eta = 0.1,
                  const double &rho = 1e-6,
                  const double &beta = 0.5,
                  const int &qmax = 128)
{
  /*
  * tsmvrRcpp regressor matrix iterate update via the gradient descent
  * method.
  *
  * Given current regressor matrix iterate B (p-by-q), design matrix
  * X (p-by-n), and response matrix Y (q-by-q), and learning rate
  * eta (eta > 0), returns the updated iterate B (p-by-q matrix)
  * via the gradient descent method.
  */
  if (type == "gd") {
    B = ht( gdB(B, Omega, S, H, X.n_rows, eta), s);
  }
  else if (type == "ls") {
    B = lsB(B, Omega, S, H, X, Y, s, eta, rho, beta, qmax);
  }
  else {
    throw std::range_error("B-step type must be 'gd' or 'ls'.");
  }
  return B;
}

// // [[Rcpp::export]]
arma::mat updateOmega(const arma::mat &B, arma::mat Omega,
                      const arma::mat &X, const arma::mat &Y,
                      const std::string &type,
                      const int &s,
                      const double &eta = 0.2,
                      const double &rho = 1e-6,
                      const double &beta = 0.5,
                      const bool &disp_min_ev = false,
                      const double &del = 1e-6,
                      const int &qmax = 128) {
  /*
  * tsmvrRcpp covariance matrix iterate update via the gradient descent
  * method.
  *
  * Given current regressor matrix iterate B (p-by-q), current
  * covariance iterate Omega (q-by-q), design matrix
  * X (p-by-n), and response matrix Y (q-by-q), and learning rate
  * eta (eta > 0), returns the updated iterate Omega (q-by-q matrix)
  * via the gradient descent method.
  */
  if (type == "gd") {
    arma::mat Omega_twidle = gdOmega(B,Omega,X,Y,eta,disp_min_ev,del);
    Omega = ht(Omega_twidle,s,true);
  }
  else if (type == "ls") {
    Omega = lsOmega(B,Omega,X,Y,s,eta,rho,beta,disp_min_ev,del,qmax);
  }
  else if (type == "min") {
    Omega = ht(minOmega(B,X,Y),s,true);
  }
  else {
    throw std::range_error("Omega-step 'type' must be 'gd' or 'min'.");
  }
  return Omega;
}

//' Truly Sparse Multivariate Regression
//'
//' Solver for multivariate regression with covariance/precision
//' matrix estimation and absolute sparsity constraints.
//'
//' The tsmvr solver works by alternating blockwise coordinate descent,
//' where in each iteration there is an update of the regressor matrix
//' (the B-step) followed by an update of the precision matrix (the Omega-step).
//' While the B-step update is always made by gradient descent,
//' for the Omega-step the user has the freedom to choose either
//' gradient descent or direct minimization. In this package
//' the first method is called 'gd-gd' and the second method is
//' called 'gd-min'. While gd-min is faster than gd-gd, for some
//' problems direct minimization causes the solution to explode in a
//' manner similar to that of gradient descent with a too large learning
//' rate. As usual, setting the learning rate too large for gradient
//' descent itself will also cause the solution to explode. The best way
//' to find a solution is to use trial and error (or some more principled
//' method) to find the largest learning rate(s) that yield convergence
//' in either gd-gd mode or gd-min mode.
//'
//' In general, \code{eta1} may be set as high as 0.1 or 0.2 for some
//' nicely behaved problems, and it is rare that any problem can
//' be solved with a larger learning rate. Similarly, \code{eta2}
//' may be set as high as 0.5 for some generous problems, and it
//' is rare that larger values will ever work on any problem. For most
//' problems \code{eta1} and \code{eta2} will need to be set to lower
//' value, anywhere from 0.1 down to 0.0001.
//'
//' The sparsity parameters \code{s1} and \code{s2} specify the
//' sparsity for B and Omega matrices as the algorithm iterates.
//' They act as regularizers, constraining the space of possible
//' solutions at each iteration. For real-world problems, the best
//' values of \code{s1} and \code{s2} need to be found by
//' cross-validation and gridsearch. \code{s2} is lower bounded by
//' \code{q}, since the covariance matrix must at least be diagonal.
//'
//' 10E-4 is usually a good value for \code{epsilon}. The author
//' rarely finds problems where smaller values of \code{epsilon} gave
//' solutions with better at out-of-sample prediction.
//' Alternatively, good predictive solutions can often be found
//' using values of 10E-3 or even 10E-2.
//'
//' For speed, tsmvr_solve is implemented in Rcpp.
//'
//' @param X design matrix (n-by-p)
//' @param Y response matrix (n-by-q)
//' @param s1 sparsity parameter for regression matrix (positive integer)
//' @param s2 sparsity parameter for covariance matrix (positive integer)
//' @param B_type type of descent for regression steps (string: 'gd')
//' @param Omega_type (string: 'gd' or 'min')
//' @param eta1 B-step learning rate (positive numeric)
//' @param eta2 Omega-step learning rate (positive numeric)
//' @param rho1 B-step learning rate (positive numeric)
//' @param rho2 Omega-step learning rate (positive numeric)
//' @param beta1 B-step learning rate (positive numeric)
//' @param beta2 Omega-step learning rate (positive numeric)
//' @param epsilon convergence parameter (positive numeric)
//' @param max_iter maximum number of iterations (positive integer)
//' @param skip iteration skip frequency for printing to screen (positive integer)
//' @param quiet whether or not to operate   (bool)
//' @param suppress whether or not to suppress warnings (bool)
//'
//' @references \insertRef{chen2016high}{tsmvr}
//'
//' @return A list of algorithm output, including:
//'
//' \code{B_hat} - final iterate of the regression matrix \cr
//' \code{Omega_hat} - final iterate of the precision matrix \cr
//' \code{objective} - final value of the objective function \cr
//' \code{B_history} - list of all regression matrix iterates \cr
//' \code{Omega_history} - list of all precision matrix iterates \cr
//' \code{objective_history} - list of the objective function values for each iteration \cr
//' \code{iterations} - number of iterations \cr
//' \code{time} - algorithm time (seconds) \cr
//' \code{Y_hat} - fitted response, given by \code{X*B_hat} \cr
//' \code{residuls} - difference between the actual and fitted responses
//'
//'
// //' @export
// [[Rcpp::export]]
List tsmvr_solve(const arma::mat &X,
                 const arma::mat &Y,
                 const int &s1, const int &s2,
                 const Rcpp::List &pars)
  {

  // Extract algorithm parameters from list "pars".
  Rcpp::List x(pars);
  const std::string B_type = as<std::string>(x["B_type"]);
  const std::string Omega_type = as<std::string>(x["Omega_type"]);
  const double eta1 = as<double>(x["eta1"]);
  const double eta2 = as<double>(x["eta2"]);
  const double lam1 = as<double>(x["lam1"]);
  const double lam2 = as<double>(x["lam2"]);
  const double del1 = as<double>(x["del1"]);
  const double del2 = as<double>(x["del2"]);
  const double del3 = 0;
  const double rho1 = as<double>(x["rho1"]);
  const double rho2 = as<double>(x["rho2"]);
  const double beta1 = as<double>(x["beta1"]);
  const double beta2 = as<double>(x["beta2"]);
  const int qmax1 = as<int>(x["qmax2"]);
  const int qmax2 = as<int>(x["qmax2"]);
  const double eps1 = as<double>(x["eps1"]);
  const double eps2 = as<double>(x["eps2"]);
  const int max_iter = as<int>(x["max_iter"]);
  const int skip = as<int>(x["skip"]);
  const bool quiet = as<bool>(x["quiet"]);
  const bool suppress = as<bool>(x["quiet"]);
  const bool disp_min_ev = as<bool>(x["disp_min_ev"]);
  const bool save_history = as<bool>(x["save_history"]);

  // Print header.
  if (!quiet) {
    Rcpp::Rcout << "Solver mode " << B_type << "-" << Omega_type << " with ";
    if (Omega_type == "min") {
      Rcpp::Rcout << "eta1 = " << eta1 << "." << endl;
    } else {
      Rcpp::Rcout << "eta1 = " << eta1 << " and eta2 = " << eta2 << "." << endl;
    }
    Rcpp::Rcout << "t\tobj\t||\u2207B/eta1||\t||\u2207\u03A9/eta2||\ttime (ms)" << endl;
  }

  // For recording history of iterates.
  field<arma::mat> BHist(max_iter);
  field<arma::mat> OmegaHist(max_iter);
  arma::vec objHist(max_iter);

  // For caching previous iterates.
  arma::mat BOld;
  arma::mat OmegaOld;
  double objOld;

  // For determining convergence.
  int itrs;
  double gamma1 = std::numeric_limits<double>::infinity();
  double gamma2 = std::numeric_limits<double>::infinity();

  // For keeping time.
  double time;
  long now;
  clock_t start = clock();  // Start the clock.

  // Constant matrices.
  const arma::mat S = X.t()*X;
  const arma::mat H = X.t()*Y;

  // Initial iterates.
  arma::mat B = initialize_B(S,H,s1,lam1,del1);
  arma::mat Omega = initialize_Omega(B,X,Y,s2,lam2,del2);
  double obj = std::numeric_limits<double>::infinity();

  // Main loop of tsmvr algorithm.
  for (int k=1; k<=max_iter; k=k+1) {

    // Cache previous iterate.
    BOld = B;
    OmegaOld = Omega;
    objOld = obj;

    // Update current iterate.
    B = updateB(B, Omega, X, Y, S, H, B_type, s1,
          eta1, rho1, beta1, qmax1);
    Omega = updateOmega(B, Omega, X, Y, Omega_type, s2,
              eta2, rho2, beta2, disp_min_ev, del3, qmax2);
    obj = objective(B, Omega, X, Y);

    // Throw error if solution diverges.
    if (obj > objOld) {
      throw std::runtime_error("Solution diverged. Try adjusting parameters.");
    }

    // Norms of differences.
    // gamma1 = pow(norm((B-BOld)/eta1,"fro"),2);
    // gamma2 = pow(norm((Omega-OmegaOld)/eta2,"fro"),2);
    gamma1 = norm((B-BOld)/eta1,"fro");
    gamma2 = norm((Omega-OmegaOld)/eta2,"fro");

    // If not quiet, print results to screen.
    if(quiet == false && k % skip == 0) {
      now = ( clock() - start ) / (double) CLOCKS_PER_SEC * 1000;
      Rcpp::Rcout <<  k << "\t"                                            \
                  << round((long)(obj*1000000.0))/1000000.0 << "\t"        \
                  << round((long)(gamma1*1000000.0))/1000000.0 << "\t"   \
                  << round((long)(gamma2*1000000.0))/1000000.0 << "\t" \
                  << round(now) << endl;
    }

    // Save iterates to history.
    if (save_history) {
      BHist(k-1) = B;
      OmegaHist(k-1) = Omega;
      objHist(k-1) = obj;
    }

    // Test for convergence.
    itrs = k;
    // gamma = std::max(gamma1, gamma2);
    if (gamma1 <= eps1 && gamma2 <= eps2) break;

  }

  // Time duration
  time = ( clock() - start ) / (double) CLOCKS_PER_SEC;

  // Warn if maximum number of iterations was reached.
  if (itrs == max_iter) {
    if (suppress == false) {
      Rcpp::warning("warning: Maximum number of iterations achieved without convergence.\n");
    }
  }

  // Statistics.
  arma::mat Y_hat = X*B;
  arma::mat res = Y-Y_hat;
  double ss = trace(res.t()*res);
  // int df = n-(p+q+q^2);    // df = # obs - (coefficents + responses +
  // size of precision matrix) [correct ?]
  // double se = ss/std::sqrt(df);  // standard error, not impelented ..

  // Clean up history objects by resizing. If maximum number of iterations is
  // reached, warn.
  field<arma::mat> BHist2(itrs);
  field<arma::mat> OmegaHist2(itrs);
  arma::vec objHist2(itrs);
  if (save_history) {
    if (itrs == max_iter) {
      BHist2 = BHist;
      OmegaHist2 = OmegaHist;
      objHist2 = objHist;
      if (suppress == false) {
        Rcpp::warning("warning: Maximum number of iterations achieved without convergence.\n");
      }
    }
    else {
      for (int k=0; k<itrs; k=k+1) {
        BHist2(k) = BHist(k);
        OmegaHist2(k) = OmegaHist(k);
        objHist2(k) = objHist(k);
      }
    }

  } else {
    BHist2.reset();
    OmegaHist2.reset();
    objHist2.reset();
  }

  // Return.
  return List::create(Named("B_hat") = B,
                      Named("Omega_hat") = Omega,
                      Named("Y_hat") = Y_hat,
                      Named("residuals") = res,
                      Named("sum_of_squares") = ss,
                      Named("num_obs") = X.n_rows,
                      Named("num_predictors") = X.n_cols,
                      Named("num_responses") = Y.n_cols,
                      Named("iterations") = itrs,
                      Named("time") = time,
                      Named("B_history") = BHist2,
                      Named("Omega_history") = OmegaHist2,
                      Named("objective") = obj,
                      Named("objective_history") = objHist2);

}
