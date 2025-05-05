import time


class Foo:
    def __init__(self, a: int, b: float, c: str | None):
        self.a = a
        self.b = b
        self.c = c

    def bar(self, x):
        print(f"Arg 'x' is {x}")
        print(f"Field 'a' is {self.a}")
        print(f"Field 'b' is {self.b}")
        print(f"Field 'c' is {self.c}")
        print("Updating field 'c' to 'new value'")
        self.c = "new value"
        print("Calling method 'baz'")
        self.baz(1, 2)

    @staticmethod
    def baz(a, b, c=None):
        if c is None:
            c = {"x": list(range(1, 6))}
        print(f"Arg 'a' is {a}")
        print(f"Arg 'b' is {b}")
        print(c)


# Instantiating objects
foo1 = Foo(a=1, b=2.0, c="xxx")
foo2 = Foo(a=1, b=2.0, c=None)

# Benchmarking creation of many Foo objects
n = int(1e5)
start_time = time.time()
foos = [Foo(a=i, b=1.5, c="xxx") for i in range(1, n + 1)]
end_time = time.time()

print(f"Elapsed time: {end_time - start_time:.2f} seconds")

print(foos[0])
print(foos[0].a)
