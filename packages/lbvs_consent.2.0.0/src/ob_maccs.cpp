#include <openbabel/fingerprint.h>
#include <openbabel/mol.h>
#include <openbabel/obconversion.h>

#include <cstdio>

#include <boost/algorithm/string/predicate.hpp>

// compute MACCS166bits fingerprints for all molecules in a
// mol2 or sdf file using openbabel

using namespace OpenBabel;

int main(int argc, char** argv) {

  if (argc != 2) {
    fprintf(stderr, "usage: ./ob_maccs FILE{.sdf|.mol2}\n");
    exit(1);
  }

  OBMol mol;
  OBConversion conv;
  char* input_file = argv[1];

  if (boost::algorithm::ends_with(input_file, ".mol2")) {
    conv.SetInFormat("mol2");
  } else if (boost::algorithm::ends_with(input_file, ".sdf")) {
    conv.SetInFormat("sdf");
  } else {
    fprintf(stderr,
            "ob_maccs: fatal: unsupported file format: %s\n", input_file);
    exit(1);
  }

  OBFingerprint* fptype = OBFingerprint::FindType("MACCS");
  //printf("#mol_name,IC50 in mol/L (0.0 means unknown),MACCS166bits\n");

  bool has_more_mols = conv.ReadFile(&mol, input_file);
  while (has_more_mols) {
    std::vector<unsigned> fptvec;
    if (fptype->GetFingerprint(&mol, fptvec)) {
      printf("%s,0.0,", mol.GetTitle());

      // FBR: hardcoded MACCS length = 166; don't know how to get it
      //      dynamically
      for (unsigned int i = 0; i < 166; ++i) {
        bool i_bit = fptype->GetBit(fptvec, i);
        i_bit ? printf("1") : printf("0");
      }
      printf("\n");
    }
    mol.Clear();
    has_more_mols = conv.Read(&mol);
  }

  return 0;
}
