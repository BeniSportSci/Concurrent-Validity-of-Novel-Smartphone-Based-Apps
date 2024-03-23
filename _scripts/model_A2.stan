data {
  int<lower=1> nSubjects;
  int<lower=1> nReps; // expected observations per subject, according to protocol
  int<lower=0> compReps[nSubjects]; // complete observations per subject
  real<lower=0> vel[nReps, nSubjects, 2]; // velocity
  real<lower=0> mean_vel; // mean imported from R to facilitate data transformation with array structure
  real<lower=0> sd_vel; // sd imported from R to facilitate data transformation with array structure
  
  
  vector[2] prior_a; // group level location prior for random intercepts
  vector[2] prior_b; // group level location prior for random slopes

  vector<lower=0>[2] prior_sigma; // model error prior
  vector<lower=0>[2] prior_tau1; // group level scale prior for random intercepts
  vector<lower=0>[2] prior_tau2; // group level scale prior for random slopes
  
  real<lower=0> prior_eta; // LKJ correlation prior
}


transformed data {
  real vel_std[nReps, nSubjects, 2];
  for (r in 1:nReps){
    for (s in 1:nSubjects){
      for (d in 1:2){
        
        vel_std[r, s, d] = (vel[r, s, d] - mean_vel) / sd_vel; // data standardization
        
      }
    }
  }
}


parameters {
  // group-level parameters
  real<lower=0> sigma;

  real alpha;
  real beta;

  // cholesky factorization
  matrix[2, nSubjects] z_u;
  cholesky_factor_corr[2] L_u;
  vector<lower=0>[2] tau_u;

}


transformed parameters {
  // matrix of correlated parameters (Cholesky factorization)
  matrix[nSubjects, 2] u = (diag_pre_multiply(tau_u, L_u) * z_u)';
  
  // calculating absolute subject level parameters; 
  vector[nSubjects] a = alpha + u[,1];
  vector[nSubjects] b = beta + u[,2];
  
}


model {
  // group-level prior
  target += normal_lpdf(sigma | prior_sigma[1], prior_sigma[2]);

  target += normal_lpdf(alpha | prior_a[1], prior_a[2]); 
  target += normal_lpdf(beta | prior_b[1], prior_b[2]);
  
  // generate uncorrelated vectors for Cholesky factorization
  target += std_normal_lpdf(to_vector(z_u));
  
  // prior for Cholesky factorization
  target += lkj_corr_cholesky_lpdf(L_u | prior_eta);
  
  // priors for scaling of correlated parameters
  target += normal_lpdf(tau_u[1] | prior_tau1[1], prior_tau1[2]); 
  target += normal_lpdf(tau_u[2] | prior_tau2[1], prior_tau2[2]);
  
  // likelihood
  for (s in 1:nSubjects){
    
    for (d in 1:2){ // d = 1: criterion (i.e., Vicon); d = 2: practical (i.e., device XYZ)
      
      if (compReps[s] > 0){ // only include subjects with 1+ reps recorded for both, the criterion & practical
        
        target += normal_lpdf(vel_std[:compReps[s], s, d] | a[s] + b[s] * (d - 1), sigma);
        
      }
      
    }
    
  }
  
}


generated quantities {
  
  real log_lik[nSubjects];
  
  real alpha_rec; // original parameter scale recovered
  real beta_rec; // original parameter scale recovered
  real<lower=0> sigma_rec; // original parameter scale recovered
  vector<lower=0>[2] tau_u_rec; // original parameter scale recovered
  
  real a_rec[nSubjects]; // original parameter scale recovered
  real b_rec[nSubjects]; // original parameter scale recovered
  
  real v_pred[nReps, nSubjects, 2]; // model-based predictions (required for R² computation)
  real residuals[nReps, nSubjects, 2]; // model residuals (required for R² computation)
  
  corr_matrix[2] Sigma_corr; // correlation matrix for PPD computation
  
  Sigma_corr = multiply_lower_tri_self_transpose(L_u);
  
  alpha_rec = alpha * sd_vel + mean_vel;
  beta_rec = beta * sd_vel;
  sigma_rec = sigma * sd_vel;
  tau_u_rec = tau_u * sd_vel;
  
  for (s in 1:nSubjects) {
    
    log_lik[s] = 0;
    
    a_rec[s] = a[s] * sd_vel + mean_vel;
    b_rec[s] = b[s] * sd_vel;
    
    // log likelihood
    
    for (d in 1:2){
      
      if (compReps[s] > 0){
        log_lik[s] += normal_lpdf(vel_std[:compReps[s], s, d] | a[s] + b[s] * (d - 1), sigma); // log likelihood for optional model comparison
      }
      
      for (r in 1:nReps) {
        
        if (r <= compReps[s]){
          v_pred[r, s, d] = a_rec[s] + b_rec[s] * vel[r, s, d];
          residuals[r, s, d] = vel[r, s, d] - v_pred[r, s, d];
        }else{
          v_pred[r, s, d] = 999;
          residuals[r, s, d] = 999;
        }
        
      }
      
    }
    
  }
  
}
