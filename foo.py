class Foo:
    a: int

    def __init__(self, a: int):
        self.a = a


foo = Foo(1)

Foo = "hello"

foo = Foo(1)
