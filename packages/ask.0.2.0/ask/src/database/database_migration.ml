module MariaDB = struct
  let questions =
    Sihl.Database.Migration.create_step
      ~label:"questions"
      {sql|
        CREATE TABLE IF NOT EXISTS `ask_questions` (
          `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
          `uuid` binary(16) NOT NULL,
          `label` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
          `help_text` varchar(512) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
          `text` varchar(512) COLLATE utf8mb4_unicode_ci NOT NULL,
          `default_value` varchar(512) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
          `validation_regex` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT '.+',
          `question_type` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT 'text',
          `max_file_size_mb` int(11) DEFAULT NULL,
          `mime_types` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
          `possible_options` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
          `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
          `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (`id`),
        UNIQUE KEY `unique_uuid` (`uuid`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
      |sql}
  ;;

  let templates =
    Sihl.Database.Migration.create_step
      ~label:"templates"
      {sql|
        CREATE TABLE IF NOT EXISTS `ask_templates` (
          `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
          `uuid` binary(16) NOT NULL,
          `label` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL,
          `description` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
          `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
          `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (`id`),
        UNIQUE KEY `unique_uuid` (`uuid`),
        UNIQUE KEY `unique_label` (`label`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
      |sql}
  ;;

  let questionnaires =
    Sihl.Database.Migration.create_step
      ~label:"questionnaires"
      {sql|
        CREATE TABLE IF NOT EXISTS `ask_questionnaires` (
          `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
          `uuid` binary(16) NOT NULL,
          `ask_template` bigint(20) unsigned NOT NULL,
          `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
          `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (`id`),
        UNIQUE KEY `unique_uuid` (`uuid`),
        KEY `ask_template` (`ask_template`),
        CONSTRAINT `ask_questionnaires_ibfk_1` FOREIGN KEY (`ask_template`) REFERENCES `ask_templates` (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
      |sql}
  ;;

  let mappings =
    Sihl.Database.Migration.create_step
      ~label:"mappings"
      {sql|
        CREATE TABLE IF NOT EXISTS `ask_template_question_mappings` (
          `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
          `uuid` binary(16) NOT NULL,
          `ask_template` bigint(20) unsigned NOT NULL,
          `ask_question` bigint(20) unsigned NOT NULL,
          `question_order` bigint(20) unsigned NOT NULL,
          `required` tinyint(1) DEFAULT '0',
          `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
          `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (`id`),
        UNIQUE KEY `unique_uuid` (`uuid`),
        UNIQUE KEY `unique_order` (`ask_template`, `question_order`),
        KEY `ask_question` (`ask_question`),
        CONSTRAINT `ask_template_question_mappings_ibfk_1` FOREIGN KEY (`ask_template`) REFERENCES `ask_templates` (`id`),
        CONSTRAINT `ask_template_question_mappings_ibfk_2` FOREIGN KEY (`ask_question`) REFERENCES `ask_questions` (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
      |sql}
  ;;

  let answers =
    Sihl.Database.Migration.create_step
      ~label:"answers"
      ~check_fk:false
      {sql|
        CREATE TABLE IF NOT EXISTS `ask_answers` (
          `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
          `uuid` binary(16) NOT NULL,
          `ask_questionnaire` bigint(20) unsigned NOT NULL,
          `ask_template_question_mapping` bigint(20) unsigned NOT NULL,
          `text` varchar(5000) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
          `storage_handle` binary(16) NULL,
          `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
          `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
          PRIMARY KEY (`id`),
          UNIQUE KEY `unique_uuid` (`uuid`),
          KEY `ask_questionnaire` (`ask_questionnaire`),
          KEY `ask_template_question_mapping` (`ask_template_question_mapping`),
          KEY `storage_handle` (`storage_handle`),
          CONSTRAINT `ask_answers_ibfk_1` FOREIGN KEY (`ask_questionnaire`) REFERENCES `ask_questionnaires` (`id`),
          CONSTRAINT `ask_answers_ibfk_2` FOREIGN KEY (`ask_template_question_mapping`) REFERENCES `ask_template_question_mappings` (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
      |sql}
  ;;

  let migration () =
    let open Sihl.Database.Migration in
    empty "attributes"
    |> add_step questions
    |> add_step templates
    |> add_step questionnaires
    |> add_step mappings
    |> add_step answers
  ;;
end
