States ={
  S,
  E,
  I,
  Cum_I,
  J,
  R
};

Initial_S = 100;
Initial_E = 0.0;
Initial_I = 1.0;
Initial_J = 0.0;
Initial_R = 0.0;

R0 = 2; # # (range 2-3)
beta;
sigma = 0.2; # (range 1-9)
alpha = 0.167; # (range 2-10)
gama = 0.167; # (no information to set uncertainty)
mu = 0.02; #(range 0.017-0.024)

Initialize{
  S = Initial_S;
  E = Initial_E;
  I = Initial_I;
  Cum_I = Initial_I;
  J = Initial_J;
  R = Initial_R;
  
  beta= R0 * alpha;  
};

Dynamics {
  N = S + E + I + J + R;
  
  dt(S)=-beta*I*S/N;
  dt(E)=beta*I*S/N-sigma*E;
  dt(I)=sigma*E-alpha*I;
  dt(J)=alpha*I-(gama+mu)*J;
  dt(R)=gama*J;
  
  dt(Cum_I)=sigma*E;
  
};

End.
