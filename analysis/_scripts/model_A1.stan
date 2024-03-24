functions {
  
  real clean_mean(vector l) { // mean that removes NAs (coded with value = 999)
    real Sum = 0;
    int Count = 0;
    for (i in 1:num_elements(l)) {
      if (l[i] < 999)
        Sum = Sum + l[i];
        Count = Count + 1;
    }
    return Sum / Count; 
  }
  
  real clean_sd(vector l) { // sd that removes NAs (coded with value = 999)
    real Mean = clean_mean(l);
    real Diff = 0;
    real Sum = 0;
    int Count = 0;
    for (i in 1:num_elements(l)) {
      if (l[i] < 999)
        Diff = (l[i] - Mean)^2;
        Sum = Sum + Diff;
        Count = Count + 1;
    }
    return sqrt(Sum / (Count - 1)); 
  }
  
}


data {
  int<lower=1> nSubjects;
  int<lower=1> nReps; // expected observations per subject, according to protocol
  int<lower=0> compReps[nSubjects]; // complete observations per subject
  matrix<lower=0>[nReps, nSubjects] vVic; // Vicon data
  matrix<lower=0>[nReps, nSubjects] vDev; // device data
  
  vector[2] prior_a; // group level location prior for random intercepts
  vector[2] prior_b; // group level location prior for random slopes

  vector<lower=0>[2] prior_sigma; // model error prior
  vector<lower=0>[2] prior_tau1; // group level scale prior for random intercepts
  vector<lower=0>[2] prior_tau2; // group level scale prior for random slopes
  
  real<lower=0> prior_eta; // LKJ correlation prior
}


transformed data {
  real mean_x = clean_mean(to_vector(vVic));
  real mean_y = clean_mean(to_vector(vDev));
  real sd_x = clean_sd(to_vector(vVic));
  real sd_y = clean_sd(to_vector(vDev));
  matrix[nReps, nSubjects] vVic_std = (vVic-mean_x)/sd_x; // data standardization
  matrix[nReps, nSubjects] vDev_std = (vDev-mean_y)/sd_y; // data standardization
}


parameters {
  // group-level parameters
  real<lower=0> sigma;

  real alpha;
  real beta;

  // Cholesky factorization
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
    
    if (compReps[s] > 0){ // only include subjects with 1+ reps recorded for both, the criterion & practical
      
      target += normal_lpdf(vDev_std[:compReps[s], s] | a[s] + b[s] * vVic_std[:compReps[s], s], sigma);
      
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
  
  real v_pred[nReps, nSubjects]; // model-based predictions (required for R² computation)
  real residuals[nReps, nSubjects]; // model residuals (required for R² computation)
  
  corr_matrix[2] Sigma_corr; // correlation matrix for PPD computation
  
  Sigma_corr = multiply_lower_tri_self_transpose(L_u);
  
  alpha_rec = alpha * sd_y + mean_y - beta * sd_y * mean_x / sd_x;
  beta_rec = beta * sd_y / sd_x;
  sigma_rec = sigma * sd_y;
  tau_u_rec = tau_u * sd_y;
  
  
  for (s in 1:nSubjects) {
    
    log_lik[s] = 0;
    
    a_rec[s] = a[s] * sd_y + mean_y - b[s] * sd_y * mean_x / sd_x;
    b_rec[s] = b[s] * sd_y / sd_x;
    
    // log likelihood
    
    if (compReps[s] > 0){
      log_lik[s] += normal_lpdf(vDev_std[:compReps[s], s] | a[s] + b[s] * vVic_std[:compReps[s], s], sigma); // log likelihood for optional model comparison
    }
    
    for (r in 1:nReps) {
      
      if (r <= compReps[s]){
        v_pred[r, s] = a_rec[s] + b_rec[s] * vVic[r, s];
        residuals[r, s] = vDev[r, s] - v_pred[r, s];
      }else{
        v_pred[r, s] = 999;
        residuals[r, s] = 999;
      }
      
    }
    
  }
  
}
