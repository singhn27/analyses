def q(u):
    return (1+u)**m
    
u = TrialFunction(V)
v = TestFunction(V)
u_k = interpolate(Constant(0.0), V)
a = inner(q(u_k)*nabla_grad(u), nabla_grad(v))*dx
f = Constant(0.0)
L = f*v*dx

u = Function(V)
eps = 1.0
tol = 1.0E-5
iter = 0
maxiter = 25
while eps > tol and iter < maxiter
    iter +=1
    solve(a == L, u, bcs)
    diff = u.vector().array() - u_k.vector().array()
    eps = numpy.linalg.norm(diff, ord=numpy.Inf)
    print "iter=%d: norm=%g" % (iter, eps)
    u_k.assign(u)