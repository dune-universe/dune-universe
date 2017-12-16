package rulebot;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication(scanBasePackages = { "me.ramswaroop.jbot", "rule" })
public class RuleSlackBotApp {

  /**
   * Entry point of the application. Run this method to start the sample bots,
   * but don't forget to add the correct tokens in application.properties file.
   *
   * @param args
   */
  public static void main(String[] args) {
    SpringApplication.run(RuleSlackBotApp.class, args);
  }
}