mod1_2_v4.jags <- "
Data{
  for (i in 1:NN) {
    for (r in 1:p) {
      Y_tp[r, tind[i], ind[i]] <- y_tp[i,r]
      Y_fp[r, tind[i], ind[i]] <- y_fp[i,r]
    }
    X[tind[i], ind[i], 1:q] <- x[i,1:q]
  }
}
model{
  for (j in 1:N) {
    # Random effects -----------------------------------------------------------
    u[j,1:2] ~ dmnorm(rep(0,2), invSigma.u)
    u_tp[j] <- u[j,1]
    u_fp[j] <- u[j,2]

    # Model for t = 1 ----------------------------------------------------------
    C_tp[1,j] ~ dbern(lambda_tp[1,j])
    logit(lambda_tp[1,j]) = b_tp[1] + inprod(X[1, j, 1:q], g_tp[1:q]) + u_tp[j]

    C_fp[1,j] ~ dbern(lambda_fp[1,j])
    logit(lambda_fp[1,j]) = b_fp[1] + inprod(X[1, j, 1:q], g_fp[1:q]) + u_fp[j]

    for (r in 1:p) {
      Y_tp[r,1,j] ~ dbern(pi_tp[r,1,j])
      logit(pi_tp[r,1,j]) = alpha_tp[1,r] + alpha_tp[2,r] * C_tp[1,j]

      Y_fp[r,1,j] ~ dbern(pi_fp[r,1,j])
      logit(pi_fp[r,1,j]) = alpha_fp[1,r] + alpha_fp[2,r] * C_fp[1,j]
    }

    # Model for t = 2,...,T ----------------------------------------------------
    for (t in 2:T.vec[j]) {
      # Latent state for aid to parents (*_tp) ---------------------------------
      C_tp[t,j] ~ dbern(lambda_tp[t, j])
      logit(lambda_tp[t, j]) = beta_tp[1] + beta_tp[2] * C_tp[t-1,j] +
        beta_tp[3] * C_fp[t-1,j] +
        inprod(X[t, j, 1:q], gamma_tp[1:q]) + u_tp[j]

      # Latent state for aid from parents (*_fp) -------------------------------
      C_fp[t,j] ~ dbern(lambda_fp[t, j])
      logit(lambda_fp[t, j]) = beta_fp[1] + beta_fp[2] * C_fp[t-1,j] +
        beta_fp[3] * C_tp[t-1,j] +
        inprod(X[t,j,1:q], gamma_fp[1:q]) + u_fp[j]

      for (r in 1:p) {
        # Modelling responses for aid to parents -------------------------------
        Y_tp[r,t,j] ~ dbern(pi_tp[r,t,j])
        logit(pi_tp[r,t,j]) = alpha_tp[1,r] + alpha_tp[2,r] * C_tp[t,j]

        # Modelling responses for aid from parents -----------------------------
        Y_fp[r,t,j] ~ dbern(pi_fp[r,t,j])
        logit(pi_fp[r,t,j]) = alpha_fp[1,r] + alpha_fp[2,r] * C_fp[t,j]
      }
    }
  }

  # Priors ---------------------------------------------------------------------
  tau.u_tp ~ dgamma(1.5, 1e-4)
  tau.u_fp ~ dgamma(1.5, 1e-4)
  sigma.u_tp <- pow(tau.u_tp, -1/2)
  sigma.u_fp <- pow(tau.u_fp, -1/2)

  rho.u ~ dnorm(mu_rho.u,tau_rho.u)T(-1,1)
  mu_rho.u ~ dunif(-1,1)
  tau_rho.u ~ dgamma(1.5, 1e-4)

  R.u[1,1] <- pow(sigma.u_tp,2)
  R.u[2,2] <- pow(sigma.u_fp,2)
  R.u[1,2] <- rho.u * sigma.u_tp * sigma.u_fp
  R.u[2,1] <- rho.u * sigma.u_tp * sigma.u_fp
  invSigma.u  ~ dwish(R.u, 2)
  Sigma.u <- inverse(invSigma.u)

  b_tp[1] ~ dnorm(0,0.01)
  b_fp[1] ~ dnorm(0,0.01)
  for (k in 1:3) {
    beta_tp[k] ~ dnorm(0,0.01)
    beta_fp[k] ~ dnorm(0,0.01)
  }
  for (k in 1:2) {
    for (r in 1:p) {
      alpha_tp[k,r] ~ dnorm(0,0.01)
      alpha_fp[k,r] ~ dnorm(0,0.01)
    }
  }
  for (k in 1:q) {
    g_tp[k] ~ dnorm(0,0.01)
    g_fp[k] ~ dnorm(0,0.01)
    gamma_tp[k] ~ dnorm(0,0.01)
    gamma_fp[k] ~ dnorm(0,0.01)
  }
}
#monitor# alpha_tp, alpha_fp, b_tp, b_fp, beta_tp, beta_fp, g_tp, g_fp, gamma_tp, gamma_fp, Sigma.u
"