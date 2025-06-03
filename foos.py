from dataclasses import dataclass
import time
from pprint import pprint as print


@dataclass
class Foo:
    x: int
    y: int
    z: int

    def bar(self):
        return self.x + self.y + self.z


N = int(3e6)

t = time.perf_counter()
# foos = [Foo(i, i, i) for i in range(N)]
for i in range(N):
    foo = Foo(i, i, i)
# print(foos[1000])
print(time.perf_counter() - t)

foo = Foo(1, 2, 3)
print(foo.__dict__)
print(Foo.__dict__)

foo.__class__.bar(foo)
Foo.__dict__["bar"]
