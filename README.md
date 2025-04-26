# containers

Fast containers for R from Rust.

Includes:

- [x] `vec`
- [x] `hashmap`
- [ ] `deque`
- [ ] `linkedlist`
- [ ] `btreemap`
- [ ] `hashset`
- [ ] `btreeset`
- [ ] `binaryheap`

## Building/testing

Run:

```R
rextendr::document()
devtools::load_all()
```

For example:

```R
> map <- hashmap$new()
> map$insert("a", 1)
> for (i in 1:10) map$insert(letters[i], i)
> map
<pointer: 0x55ee10a3a200>
attr(,"class")
[1] "hashmap"
> map$get("c")
[1] 3
```
