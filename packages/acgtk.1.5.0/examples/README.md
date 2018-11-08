# ACGtk: an ACG development toolkit.

**ACGtk** is a software package ([2008-2018 INRIA](http://www.inria.fr)©) for the development of abstract categorial grammars. This distribution provides two executables (possibly with the `.opt` extension, see the (INSTALL)[INSTALL] file: `acgc` and `acg` (or, instead, their native counterparts: `acgc.opt` and `acg.opt`).

It is distributed with the *CeCILL* license (see the [LICENSE](LICENSE.en) file or http://www.cecill.info). Contributors are listed in the [AUTHORS.md](AUTHORS.md) file.

A list of related publications is available at the [ACG web page](http://calligramme.loria.fr/acg).


# Examples

We suppose that the executables are `acgc` and `acg` (they could have been `acgc.opt` or `acg.opt`).

This directory provide the following files:
* `strings.acg`: A file containing a very simple signature (with definitions). You can use it as follows:
	
	```bash
	acgc strings.acg
	```
	
	It will say that it correctly parse the file. You can also run:
	
	```bash
	acgc -i strings.acg
	```
	
	then, on the prompt, enter the following term:
	
	```
	every + dog ! chases + a + cat : string
	```
	
	it does not complain, neither with the term:
	
	```
	lambda x. every + x : string -> string
	```
	
	But with the term:
	```
	lambda x.every x : string -> string
	```
	
	you will have a typing error.

* `montague.acg`: A file containing a very simple signature that illustrates (very basic) Montague semantics

* `montague-script`: A file providing commands that you can run in `acg`. You can also run it with the following command:	

	```bash
	acg montague-script
	```

	(press return after each command).

* `tag.acg`: A file containing various signatures and lexicons to model TAG in ACG. You can run `acgc` on it.

* `tag-script`: This file provides (commented) commands that you can run in `acg`. You can also run it with the following command:

	```bash
	acg tag-script
	```
	(press return after each command).
