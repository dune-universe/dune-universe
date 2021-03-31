module MariaDB = struct
  let service_mapping =
    Sihl.Database.Migration.create_step
      ~label:"service_mapping"
      {sql|
        CREATE TABLE IF NOT EXISTS `ask_integration_service_mappings` (
          `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
          `uuid` binary(16) NOT NULL,
          `member_uuid` binary(16) NOT NULL,
          `member_label` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
          `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (`id`),
          UNIQUE KEY `unique_uuid` (`uuid`)
          ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
          |sql}
  ;;

  let questionnaire_mapping =
    Sihl.Database.Migration.create_step
      ~label:"questionnaire_mapping"
      {sql|
        CREATE TABLE IF NOT EXISTS `ask_integration_questionnaire_mappings` (
          `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
          `ask_integration_service_mapping` bigint(20) unsigned NOT NULL,
          `questionnaire_label` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
          `questionnaire` binary(16) NOT NULL,
          `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
          `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (`id`),
        KEY `questionnaire` (`questionnaire`),
        CONSTRAINT `ask_integration_questionnaire_mappings_ibfk_1`
          FOREIGN KEY (`ask_integration_service_mapping`)
          REFERENCES `ask_integration_service_mappings` (`id`)
          ON DELETE CASCADE
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
      |sql}
  ;;

  let migration () =
    let open Sihl.Database.Migration in
    empty "ask-integrator" |> add_step service_mapping |> add_step questionnaire_mapping
  ;;
end
