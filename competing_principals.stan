
functions {
  real reduce_lpmf(int[] slice, int start, int end,
                   int[,] Y, int[,] Y_bin, vector theta, vector alpha, vector beta,
                   real probVoteDisagree, real probVoteAgree,
                   int[,] lead, int[] party, int J, int P) {
    real lp = 0;
    for (i in start:end) {
      for (j in 1:J) {
        if (i <= P) {
          real CJR = Phi(beta[j] * theta[i] - alpha[j]);
          int p = lead[party[i], j];
          real q1 = pow(probVoteDisagree * (1 - CJR), p) * pow(probVoteAgree * (1 - CJR), 1 - p);
          real q3 = pow(probVoteAgree * CJR, p) * pow(probVoteDisagree * CJR, 1 - p);
          real q2 = 1 - q1 - q3;
          vector[3] q = [q1, q2, q3]';
          lp += categorical_lpmf(Y[i, j] | q);
        } else {
          int i_bin = i - P;
          lp += bernoulli_lpmf(Y_bin[i_bin, j] | Phi(beta[j] * theta[i] - alpha[j]));
        }
      }
    }
    return lp;
  }
}

data {
  int<lower=1> N;
  int<lower=1> P;
  int<lower=1> J;
  array[P, J] int<lower=1, upper=3> Y;
  array[N-P, J] int<lower=0, upper=1> Y_bin;
  array[P] int<lower=1, upper=2> party;
  array[2] int<lower=1> fix_bill;
  array[2, J] int<lower=0, upper=1> lead;
}

parameters {
  vector[N] theta;
  vector[J] alpha;
  vector[J] beta;
  real<lower=0, upper=1> delta1;
  real<lower=0, upper=1> delta2;
}

transformed parameters {
  real probVoteDisagree = delta1;
  real probVoteAgree = delta2;
}

model {
  delta1 ~ beta(1,1);
  delta2 ~ beta(1,1);
  alpha ~ normal(0, 2);
  beta[fix_bill[1]] ~ normal(4, 0.001);
  beta[fix_bill[2]] ~ normal(-4, 0.001);
  for (j in 1:J) {
    if (j != fix_bill[1] && j != fix_bill[2])
      beta[j] ~ normal(0, 2);
  }
  theta ~ normal(0, 1);

  target += reduce_sum(reduce_lpmf, rep_array(0, N), 1,
                       Y, Y_bin, theta, alpha, beta,
                       probVoteDisagree, probVoteAgree,
                       lead, party, J, P);
}

