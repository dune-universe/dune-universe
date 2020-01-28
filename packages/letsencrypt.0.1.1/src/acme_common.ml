type directory_t = {
  directory   : Uri.t;
  new_authz   : Uri.t;
  new_reg     : Uri.t;
  new_cert    : Uri.t;
  revoke_cert : Uri.t;
}

let letsencrypt_url = Uri.of_string
    "https://acme-v01.api.letsencrypt.org/directory"

let letsencrypt_staging_url = Uri.of_string
    "https://acme-staging.api.letsencrypt.org/directory"
