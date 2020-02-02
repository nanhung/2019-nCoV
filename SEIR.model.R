States ={
  S,
  I,
  R,
};

Outputs = {
  Getting_ill_PD,
  Recovering_PD,
  Reproduction_Rate,
  N
};

Initial_S = 1000000;
Initial_I = 1.0;
Initial_R = 0.0;
Contacts_PD = 4.0;
Infectiousness = 0.25;
Average_Duration = 4.0;

Initialize{
  S = Initial_S;
  E = Initial_E;
  I = Initial_I;
  H = Initial_H;
}

Dynamics{
  N = S + E + I + R;
  
  Getting_ill_PD = Contacts_PD*(I/N)*Infectiousness*S;
  Recovering_PD = I/Average_Duration;
    
  dt(S) = -Getting_ill_PD;
  dt(E) = Getting_ill_PD - Quarantine_PD;
  dt(I) = Quarantine_PD;
}