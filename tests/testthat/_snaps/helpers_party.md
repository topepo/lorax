# build_partysplit validates inputs

    Code
      build_partysplit(NA, 5.5)
    Condition
      Error in `build_partysplit()`:
      ! Invalid varid for partysplit: NA

---

    Code
      build_partysplit(integer(0), 5.5)
    Condition
      Error in `build_partysplit()`:
      ! Invalid varid for partysplit:

---

    Code
      build_partysplit(1, NA)
    Condition
      Error in `build_partysplit()`:
      ! Invalid threshold for partysplit: NA

---

    Code
      build_partysplit(1, numeric(0))
    Condition
      Error in `build_partysplit()`:
      ! Invalid threshold for partysplit:

---

    Code
      build_partysplit(1, "5.5")
    Condition
      Error in `build_partysplit()`:
      ! Invalid threshold for partysplit: "5.5"

# create_party_object validates inputs

    Code
      create_party_object(list(id = 1), data)
    Condition
      Error in `create_party_object()`:
      ! `node` must be a partynode object.

---

    Code
      create_party_object(node, list(x = 1:10))
    Condition
      Error in `create_party_object()`:
      ! `data` must be a data.frame.

