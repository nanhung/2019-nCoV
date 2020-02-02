Integrate(Lsodes, 1e-9, 1e-9, 1)
MonteCarlo ("", 500, 1111);

Distrib(R0, Uniform, 2, 3);
Distrib(sigma, Gamma, 3.69, 8.41);
Distrib(alpha, Uniform, 0.2, 0.5);

Simulation { 
  
  PrintStep (I, 0, 100, 1);
  
};
End.
