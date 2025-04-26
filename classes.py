from dataclasses import dataclass
import time
import pandas as pd
import cProfile


@dataclass
class Foo:
    a: int
    b: float
    c: str

    def bar(self, x):
        print(f"Arg 'x' is {x}")
        print(f"Field 'a' is {self.a}")
        print(f"Field 'b' is {self.b}")
        print(f"Field 'c' is {self.c}")
        print("Updating field 'c' to 'new value'")
        self.c = "new value"

    @staticmethod
    def baz(a, b, c=pd.DataFrame({"x": range(1, 6)})):
        print(f"Arg 'a' is {a}")
        print(f"Arg 'b' is {b}")
        print(c)


# Instantiate and access attributes
foo = Foo(a=1, b=2.0, c="xxx")
print(foo.a)
print(foo.b)
print(foo.c)

Foo.baz(1, 2)
foo.bar(10)
print(foo.c)

# Benchmark creation
n = int(1e5)

start_time = time.time()
foos = [Foo(a=1, b=1.5, c="xxx") for _ in range(n)]
print(f"Elapsed time: {time.time() - start_time:.2f} seconds")


# Profiling object creation with varying `a`
def create_foos():
    [Foo(a=i, b=1.5, c="xxx") for i in range(1, n + 1)]


cProfile.run("create_foos()")
cProfile.run("Foo(a=1, b=1.5, c='xxx')")
