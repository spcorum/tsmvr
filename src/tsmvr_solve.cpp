#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// #include <ctime>
// #include <cmath>

using namespace Rcpp;
using namespace arma;
using namespace std;

arma::mat ppmat(const arma::mat &X) {
    /*
     * Positive part of matrix. Given matrix x, is positive part
     * is returned.
     */
    return clamp(X,0,X.max());
}

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


arma::mat mvLasso(const arma::mat &X, const arma::mat &Y, const double &lam) {
    /*
     * Solution to multivariate Lasso problem. Given matrices X (p-by-
     * n) and Y (q-by-n) and numeric constant lam, returns the Lasso
     * solution to B (p-by-q)
     *
     * Multivariate Lasso problem: min{ 1/n ||Y-X*B||^2 + lam*|B|_11 }
     *
     * The multivariate Lasso solution is calculated by soft-
     * thresholding the ordinary least squares solution.
    */
    return st(solve(X.t()*X,X.t()*Y,solve_opts::fast),lam);
}

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
        throw "Sparsity parameter s cannot be less than zero.";
    }
    if (s > N) {
        // Rcpp::Rcout << "case of s2 = " << s << " > N = " << N << endl;
        throw "Sparsity parameter s cannot larger than the number of matrix entries.";
    }
    if (s == 0) {
        X.zeros();
    }
    else if (s < N) {

        int p = N - s;                      // p is # of elements to set to 0
        arma::mat absX = abs(X);
        vec x = sort(vectorise(absX));      // x = sorted vectorized abs(X)
        X.elem(find(absX < x(p))).zeros();  // find elements of abs(X) <=
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

arma::mat SigmaR(const arma::mat &B, const arma::mat &X,
                 const arma::mat &Y) {
   /*
    * Regression covariance matrix.
    *
    * Given regressor matrix B (p-by-q), design matrix X (p-by-n),
    * and response matrix Y (q-by-n), returns the value of the tsmvrRcpp
    * regression covariance matrix (q-by-q).
    *
    *       SigmaR = 1/n (Y-X*B)^2
    */
    arma::mat A = Y-X*B;
    return A.st()*A/X.n_rows;
}

arma::mat minB(const arma::mat &S, const arma::mat &H) {
    /*
     * Direct minimization of tsmvrRcpp objective function with respect to
     * to the regressor matrix R.
     *
     * Given matrices S and H, returns B the linear solution to SB=H.
     *
     */
    return solve(S,H);
}

arma::mat minOmega(const arma::mat &B, const arma::mat &X, const arma::mat &Y) {
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
    return inv_sympd(A.st()*A/X.n_rows);
}

arma::mat initializeB(const arma::mat &S, const arma::mat &H, const int &s,
                const double &lam = 0.1) {
   /*
    * This function intializes in the initial iterates B
    * for iterative hard threholding for multivariate
    * regression. It takes design matrix p-by-p predictor matrix
    * S = X'X, p-by-q matrix H = X'Y, sparsity parameter integer
    * s >= 0, and Lasso paramter double lam > 0.
    */
    return ht(st(solve(S,H,solve_opts::fast),lam),s);
}

arma::mat initializeOmega(const arma::mat &B0, const arma::mat &X, const arma::mat &Y,
                    const int &s, const double &lam = 0.1) {
   /*
    * This function intializes in the initial iterates B
    * for iterative hard threholding for multivariate
    * regression. It takes p-by-q initial iterate regrssor matrix B0,
    * design matrix n-by-p predictor matrix X, n-by-q matrix Y,
    * sparsity parameter integer s >= 0, and Lasso paramter double
    * lam > 0.
    */
    return ht(st(minOmega(B0,X,Y),lam),s);
}

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

arma::mat dgdB(const arma::mat &B, const arma::mat &Omega, const arma::mat &S, const arma::mat &H,
         const int &n)
    {
    /*
    * Marginal derivative of tsmvrRcpp objective function with respect to
    * regressor matrix B.
    *
    * Given regressor matrix B (p-by-q), covariance matrix Omega (q-by-
    * n), and matrices S = X'X (p-by-p) and H = Y'X (q-by-q), returns
    * the derivative of the tsmvrRcpp objective function with respect to B
    * (p-by-q matrix).
    *
    *       dgdB = 2/n (S*B-H)*Omega
    */
    return 2*(S*B-H)*Omega/n;
}

arma::mat dgdOmega(const arma::mat &B, const arma::mat &Omega, const arma::mat &X,
             const arma::mat &Y)
    {
    /*
    * Marginal derivative of tsmvrRcpp objective function with respect to
    * the covariance matrix Omega.
    *
    * n), design matrix X (p-by-n), and response matrix (q-by-n),
    * returns the marginal derivative of the tsmvrRcpp objective function as a
    * with respect to Omega (q-by-q matrix).
    *
    *       dgdOmega = 1/n ||X*B-Y||^2 - inv(Omega)
    */
    arma::mat A = X*B-Y;
    arma::mat temp = A.st()*A/X.n_rows-inv_sympd(Omega);
    return temp;
}

arma::mat cgd(arma::mat &B0, const arma::mat &S, const arma::mat &H, int max_iter=100,
        double epsilon=1, bool quiet=false) {

    /*
     * Conjugate gradient descent method for solving the matrix
     * equation SB = H.
     *
     * Inputs:
     *  B, initial guess of solution (p-by-q matrix)
     *  S, p-by-p matrix
     *  H, p-by-q matrix
     *  max_iter, number of iterations (int)
     *
     * Returns:
     *  B, solution (p-by-q matrix)
     */

    // Initialize.
    double alpha;
    double beta;
    double gamma;
    arma::mat B = B0;
    arma::mat R = H-S*B;
    arma::mat P = R;
    arma::mat Rold;

    // Print header.
    if (!quiet) Rcpp::Rcout << "t\t||R||" << endl;

    // CGD algorithm for solving S*B = H.
    for (int k=1; k<=max_iter; k=k+1) {

        Rold = R;
        alpha = trace(Rold.st()*Rold)/trace(P.st()*S*P);
        B = B + alpha*P;
        R = R - alpha*S*P;

        // Test for convergence.
        gamma = norm(R,"fro");
        if (!quiet) Rcpp::Rcout << k << "\t" << gamma << endl;
        if (gamma < epsilon) break;

        beta = trace(R.st()*R)/trace(Rold.st()*Rold);
        P = R + beta*P;

    }

    return B;
}

arma::mat updateB(arma::mat B, const arma::mat &Omega, const arma::mat &X, const arma::mat &Y,
            const arma::mat &S, const arma::mat &H, const String &type = "gd",
            const double &eta = 0.01
            // , const int &cgdstep = 100, const int &cgdeps = 1
            )
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
        B = B - eta*dgdB(B,Omega,S,H,X.n_rows);
    }
    // else if (type == "cgd") {
    //     B = cgd(B,S,H,cgdstep,cgdeps,true);
    // }
    // else if (type == "bfgs") {
        // B = bfgs(B,S,H);
    // }
    // else if (type == "min") {
    //     B = minB(S,H);
    // }
    // else {
    //     throw "B-step type must be 'gd', 'cg', or 'bfgs'.";
    // }
    else {
        throw "B-step type must be 'gd'.";
    }
    return B;
}

arma::mat updateOmega(const arma::mat &B, arma::mat Omega, const arma::mat &X, const arma::mat &Y,
                const String &type = "gd", const double &eta = 0.01)
    {
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
        Omega = Omega - eta*dgdOmega(B,Omega,X,Y);
    }
    else if (type == "min") {
        Omega = minOmega(B,X,Y);
    }
    else {
        throw "Omega-step 'type' must be 'gd' or 'min'.";
    }
    return Omega;
}

//' Truly Sparse Multivariate Regression
//'
//' Solver for multivariate regression with covariance/precision
//' matrix estimation and absolute sparsity contraints.
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
//' They act as regularizers, contraining the space of possible
//' solutions at each iteration. For real-world problems, the best
//' values of \code{s1} and \code{s2} need to be found by
//' cross-validation and gridsearch. \code{s2} is lower bounded by
//' \code{q}, since the covariance matrix must at least be diagonal.
//'
//' 10E-4 is usually a good value for \code{epsilon}. The author
//' rarely finds problems where smaller values of \code{epsilon} gave
//' solutions with better at out-of-sample prediction.
//' Altertativley, good predictively solutions can often be found
//' using values of 10E-3 or even 10E-2.
//'
//' For speed, tsmvr_solve is implemented in Rcpp.
//'
//' @param X design matrix (n-by-p)
//' @param Y response matrix (n-by-q)
//' @param s1 sparsity parameter for regression matrix (postive integer)
//' @param s2 sparsity parameter for covariance matrix (positive integer)
//' @param B_type type of descent for regression steps (string: 'gd')
//' @param Omega_type (string: 'gd' or 'min')
//' @param eta1 B-step learning rate (positive numeric)
//' @param eta2 Omega-step learning rate (positive numeric)
//' @param epsilon convergence parameter (positve numeric)
//' @param max_iter maximum number of iterations (positive integer)
//' @param skip iteration skip frequency for printing to screen (postive integer)
//' @param quiet bool
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
//' @export
// [[Rcpp::export]]
List tsmvr_solve(const arma::mat &X, const arma::mat &Y, const int &s1,
                 const int &s2,
                 const String &B_type = "gd", const String &Omega_type = "gd",
                 const double &eta1 = 0.001, const double &eta2 = 0.001,
                 const double &epsilon = 1e-4, const int &max_iter = 1000,
                 const int &skip = 10, const bool &quiet = false) {


    // Initialize objects.

    // const int n = X.n_rows;
    // const int p = X.n_cols;
    // const int q = Y.n_cols;

    const arma::mat S = X.t()*X;
    const arma::mat H = X.t()*Y;
    arma::mat BOld;
    arma::mat OmegaOld;

    double gamma = std::numeric_limits<double>::infinity();
    double obj = std::numeric_limits<double>::infinity();
    double objOld;

    double dBNorm;
    double dOmegaNorm;

    double time;
    long now;
    int itrs;

    double ss;
    // int df;
    // double se;
    arma::mat fv;
    arma::mat res;

    field<arma::mat> BHist(max_iter);
    field<arma::mat> OmegaHist(max_iter);
    field<double> objHist(max_iter);

    // string BStep;
    // string OmegaStep;

    // Print header.

    /*
      if (B_type == 'gd') {
         BStep = "B-step: gradient descent (eta1=" + to_string(eta1) \
          + ")";
      }
      if (Omega_type == 'gd') {
          OmegaStep = "Omega-step: gradient descent (eta2=" + \
              to_string(eta2) + ")";
      } else if (Omega_type == 'min') {
          OmegaStep = "Omega-step: direct minimization";
      }
    */

    if (!quiet) {
        Rcpp::Rcout << "Truly Sparse multivariate regression." << endl;
        /* Rcpp::Rcout << "B-step: gradient descent (eta1=" << eta1 \
             << ") | Omega-step: gradient descent (eta2=" << eta2 \
             << ")" << endl;

        cout << B_type << "|" << Omega_type << endl;
        Rcpp::Rcout << "t\tobj\t||\u0394B||\t\t||\u0394\u03A9||\ttime (ms)" << endl;
         */
    }

    // Start clock.
    clock_t start = clock();

    // Initial iterates.
    arma::mat B = initializeB(S,H,s1);
    arma::mat Omega = initializeOmega(B,X,Y,s2);

    // Main loop of tsmvr algorithm.
    for (int k=1; k<=max_iter; k=k+1) {

        // Cache previous iterate.
        BOld = B;
        OmegaOld = Omega;
        objOld = obj;

        // Update current iterate.
        // B = ht(updateB(B,Omega,X,Y,S,H,B_type,eta1,cgdstep,cgdeps),s1);
        B = ht(updateB(B,Omega,X,Y,S,H,B_type,eta1),s1);
        Omega = ht(updateOmega(B,Omega,X,Y,Omega_type,eta2),s2);
        obj = objective(B,Omega,X,Y);

        // Throw error if solution diverges.
        if (obj > objOld) {
            throw std::runtime_error("Solution diverged.");
        }

        dBNorm = norm(B-BOld,"fro");
        dOmegaNorm = norm(Omega-OmegaOld,"fro");

        // Print results to screen if not quiet.
        if(quiet == false && k % skip == 0) {
            now = ( clock() - start ) / (double) CLOCKS_PER_SEC * 1000;
            Rcpp::Rcout <<  k << "\t" \
                 << round((long)(obj*1000000.0))/1000000.0 << "\t" \
                 << round((long)(dBNorm*1000000.0))/1000000.0 << "\t" \
                 << round((long)(dOmegaNorm*1000000.0))/1000000.0 << "\t" \
                 << round(now) << endl;
        }

        // Save iterates to history.
        BHist(k-1) = B;
        OmegaHist(k-1) = Omega;
        objHist(k-1) = obj;

        // Test for convergence.
        itrs = k;
        gamma = std::max(dBNorm, dOmegaNorm);
        if (gamma < epsilon) break;

    }

    // Time duration
    time = ( clock() - start ) / (double) CLOCKS_PER_SEC;

    // Statistics
    fv = X*B;
    res = Y-fv;
    ss = norm(fv,"fro");
    // df = n-(p+q+q^2);    // df = # obs - (coefficents + responses +
                            //  size of precision matrix) [correct ?]
    // se = ss/std::sqrt(df);  // standard error, not impelented ..

    // Warn if maximum iterations reached & resize history fields
    // with the same control flow.
    field<arma::mat> BHist2(itrs);
    field<arma::mat> OmegaHist2(itrs);
    field<double> objHist2(itrs);
    if (itrs == max_iter) {
        Rcpp::Rcout << "Warning: maximum number of iterations achieved without convergence." \
        << endl;
        BHist2 = BHist;
        OmegaHist2 = OmegaHist;
        objHist2 = objHist;
    }
    else {
        for (int k=0; k<itrs; k=k+1) {
            BHist2(k) = BHist(k);
            OmegaHist2(k) = OmegaHist(k);
            objHist2(k) = objHist(k);
        }
    }

    // Rcpp::Rcout << BHist2 << endl;

    // Return.
    return List::create(Named("B_hat") = B,
                        Named("Omega_hat") = Omega,
                        Named("objective") = obj,
                        Named("iterations") = itrs,
                        Named("time") = time,
                        Named("Y_hat") = fv,
                        Named("residuals") = res,
                        Named("B_history") = BHist2,
                        Named("Omega_history") = OmegaHist2,
                        Named("objective_history") = objHist2);
}
