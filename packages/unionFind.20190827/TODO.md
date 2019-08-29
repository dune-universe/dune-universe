* In `StoreVector`, allow creating an initial store of size other than zero,
  so as to get rid of the log factor caused by repeated enlarging.

* Provide an implementation `StoreArray` based on a non-extensible array.

* Provide an implementation `StorePersistentArray` based on JCF's persistent
  arrays.

* Add some tests.
