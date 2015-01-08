from dolfin import *

mesh = Mesh("man_stand-694x424x1844-front")
V = FunctionSpace(mesh, "Lagrange", 1)

def boundary(x):
    return x[0] < DOLFIN_EPS or x[0] > 1.0 - DOLFIN_EPS
    
u0 = Constant(0.0)
bc = DirichletBC(V, u0, boundary)

u = TrialFunction(V)
v = TestFunction(V)
f = Expression("10*exp(-(pow(x[0] - 0.5, 2) + pow(x[1] - 0.5, 2)) / 0.02)")
g = Expression("sin(5*x[0])")
a = inner(grad(u), grad(v))*dx
L = f*v*dx + g*v*ds

u = Function(V)
solve(a == L, u, bc)

file = File("poissonx.pvd")
file << u

plot(u, interactive=True)