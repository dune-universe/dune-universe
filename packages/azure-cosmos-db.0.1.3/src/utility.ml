(* let print_bytes id bytes = *)
(*   let _ = print_endline (id ^ " length: " ^ (string_of_int (Bytes.length bytes))) in *)
(*   let print_n n = print_endline (id ^ string_of_int n ^ ": " ^ (string_of_int (Char.code (Bytes.get bytes n)))) in *)
(*   let _ = print_n 0 in *)
(*   let _ = print_n 1 in *)
(*   let _ = print_n 2 in *)
(*   let _ = print_n 3 in *)
(*   let _ = print_n 4 in *)
(*   () *)

(*let date _ =
  let expected_result = "Wed, 16 May 2018 20:08:23 +0000" in

*)

let string_replace old_value new_value string =
  let regexp = Str.regexp_string old_value in
  let result = Str.global_replace regexp new_value string in
  result

let x_ms_date time =
  let s = Netdate.mk_mail_date time in
  string_replace "+0000" "GMT" s

let string_of_bool = function true -> "true" | false -> "false"

let authorization_token_using_master_key verb resource_type resource_id date master_key =
  let key =  Base64.decode_exn master_key in
  let text =
    (String.lowercase_ascii verb) ^ "\n" ^
    (String.lowercase_ascii resource_type) ^ "\n" ^
    resource_id ^ "\n" ^
    (String.lowercase_ascii date) ^ "\n" ^
     "" ^ "\n"
  in
  let body = Bytes.of_string text in
  let signature =
    let hash = Cryptokit.MAC.hmac_sha256 key in
    hash#add_substring body 0 (Bytes.length body);
    Base64.encode_exn hash#result
  in
  let master_token = "master" in
  let token_version = "1.0" in
  let result = Uri.pct_encode ~component:`Userinfo ("type=" ^ master_token ^ "&ver=" ^ token_version ^ "&sig=" ^ signature) in
  let result2 = string_replace "%3D" "%3d" result in
  let result3 = string_replace "%2B" "%2b" result2 in
  let result4 = string_replace "%2F" "%2f" result3 in
  (* let regexp_3d = Str.regexp_string "%3D" in *)
  (* let result2 = Str.global_replace regexp_3d "%3d" result in *)
  (* let regexp_2b = Str.regexp_string "%2B" in *)
  (* let result3 = Str.global_replace regexp_2b "%2b" result2 in *)
  result4
(*
C#:

string GenerateAuthToken(string verb, string resourceType, string resourceId, string date, string key, string keyType, string tokenVersion)
{
    var hmacSha256 = new System.Security.Cryptography.HMACSHA256 { Key = Convert.FromBase64String(key) };

    verb = verb ?? "";
    resourceType = resourceType ?? "";
    resourceId = resourceId ?? "";

    string payLoad = string.Format(System.Globalization.CultureInfo.InvariantCulture, "{0}\n{1}\n{2}\n{3}\n{4}\n",
            verb.ToLowerInvariant(),
            resourceType.ToLowerInvariant(),
            resourceId,
            date.ToLowerInvariant(),
            ""
    );

    byte[] hashPayLoad = hmacSha256.ComputeHash(System.Text.Encoding.UTF8.GetBytes(payLoad));
    string signature = Convert.ToBase64String(hashPayLoad);

    return System.Web.HttpUtility.UrlEncode(String.Format(System.Globalization.CultureInfo.InvariantCulture, "type={0}&ver={1}&sig={2}",
        keyType,
        tokenVersion,
        signature));
}


payLoadBytes;
{byte[52]}
        [0]: 103
        [1]: 101
        [2]: 116
        [3]: 10
        [4]: 100
        [5]: 98
        [6]: 115
        [7]: 10
        [8]: 100
        [9]: 98
        [10]: 115
        [11]: 47
        [12]: 84
        [13]: 111
        [14]: 68
        [15]: 111
        [16]: 76
        [17]: 105
        [18]: 115
        [19]: 116
        [20]: 10
        [21]: 116
        [22]: 104
        [23]: 117
        [24]: 44
        [25]: 32
        [26]: 50
        [27]: 55
        [28]: 32
        [29]: 97
        [30]: 112
        [31]: 114
        [32]: 32
        [33]: 50
        [34]: 48
        [35]: 49
        [36]: 55
        [37]: 32
        [38]: 48
        [39]: 48
        [40]: 58
        [41]: 53
        [42]: 49
        [43]: 58
        [44]: 49
        [45]: 50
        [46]: 32
        [47]: 103
        [48]: 109
        [49]: 116
        [50]: 10
        [51]: 10

> payLoad;
"get\ndbs\ndbs/ToDoList\nthu, 27 apr 2017 00:51:12 gmt\n\n"
> hashPayLoad;
{byte[32]}
        [0]: 115
        [1]: 79
        [2]: 79
        [3]: 17
        [4]: 82
        [5]: 107
        [6]: 130
        [7]: 157
        [8]: 174
        [9]: 65
        [10]: 25
        [11]: 43
        [12]: 247
        [13]: 126
        [14]: 36
        [15]: 21
        [16]: 180
        [17]: 234
        [18]: 132
        [19]: 28
        [20]: 156
        [21]: 237
        [22]: 53
        [23]: 107
        [24]: 220
        [25]: 225
        [26]: 242
        [27]: 170
        [28]: 91
        [29]: 190
        [30]: 115
        [31]: 231
> signature;
"c09PEVJrgp2uQRkr934kFbTqhByc7TVr3OHyqlu+c+c="
> String.Format (System.Globalization.CultureInfo.InvariantCulture, "type={0}&ver={1}&sig={2}",
            keyType,
            tokenVersion,
            signature);
Identifier expected, `string' is a keyword
> s;
"type=master&ver=1.0&sig=c09PEVJrgp2uQRkr934kFbTqhByc7TVr3OHyqlu+c+c="
>

Node.js:

var crypto = require("crypto");

function getAuthorizationTokenUsingMasterKey(verb, resourceType, resourceId, date, masterKey) {
    var key = new Buffer(masterKey, "base64");

    var text = (verb || "").toLowerCase() + "\n" +
               (resourceType || "").toLowerCase() + "\n" +
               (resourceId || "") + "\n" +
               date.toLowerCase() + "\n" +
               "" + "\n";

    var body = new Buffer(text, "utf8");
    var signature = crypto.createHmac("sha256", key).update(body).digest("base64");

    var MasterToken = "master";

    var TokenVersion = "1.0";

    return encodeURIComponent("type=" + MasterToken + "&ver=" + TokenVersion + "&sig=" + signature);
}
*)
