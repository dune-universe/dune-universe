package rule;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import com.google.gson.JsonElement;

import me.ramswaroop.jbot.core.slack.Bot;
import me.ramswaroop.jbot.core.slack.Controller;
import me.ramswaroop.jbot.core.slack.EventType;
import me.ramswaroop.jbot.core.slack.models.Event;
import me.ramswaroop.jbot.core.slack.models.Message;

/**
 * The RuleBot Slack Bot
 *
 */
@Component
public class RuleSlackBot extends Bot {

  private static final Logger logger = LoggerFactory
      .getLogger(RuleSlackBot.class);

  /**
   * Slack token from application.properties file. You can get your slack token
   * next <a href="https://my.slack.com/services/new/bot">creating a new bot</a>
   * .
   */
  @Value("${slackBotToken}")
  private String slackToken = "../cred.json";

  @Value("${credFile}")
  private String credFile = "../wsconfig.json";

  @Value("${wsConfig}")
  private String wsConfig;

  @Override
  public String getSlackToken() {
    return slackToken;
  }

  @Override
  public Bot getSlackBot() {
    return this;
  }

  private class ConversationThread {
    final private String thread_ts;
    final private RulebotConversation conversation;

    public ConversationThread(final String thread_ts, RulebotConversation conversation) {
      this.thread_ts = thread_ts;
      this.conversation = conversation;
    }

    public String getThread_ts() {
      return thread_ts;
    }

    public void sendUserInput(WebSocketSession session, Event event,
        String userInput) throws IOException {

      if (userInput == null) {
        userInput = "";
      }

      if (logger.isInfoEnabled()) {
        logger.info("H: " + userInput);
      }

      final List<RulebotMessage> responses = conversation.send(userInput);
      for (RulebotMessage msg:responses) {
    	  if (logger.isInfoEnabled()) {
    		  logger.info("C: " + msg.text);
    	  }

    	  final Message message = new Message(msg.text);
    	  message.setThreadTs(getThread_ts());
    	  reply(session, event, message);
      }
    }

  }

  // this is totally going to leak memory :-)
  private final Map<String, ConversationThread> conversations = new HashMap<>();

  /**
   * Invoked when the bot receives a direct mention (@botname: message) or a
   * direct message. NOTE: These two event types are added by jbot to make your
   * task easier, Slack doesn't have any direct way to determine these type of
   * events.
   *
   * @param session
   * @param event
 * @throws IOException 
   */
  @Controller(events = { EventType.DIRECT_MENTION, EventType.DIRECT_MESSAGE })
  public void onReceiveDM(WebSocketSession session, Event event) throws IOException {
    final String ts = event.getTs();
    final String thread_ts = event.getThreadTs();

    if (thread_ts != null) {
      final ConversationThread conversationThread;
      synchronized (this) {
        conversationThread = conversations.get(thread_ts);
      }
      if (conversationThread != null) {
        assert(!thread_ts.equals(ts));
        // TODO: should I remove the @rulebot, if present?
        continueConversation(conversationThread, session, event);
        return;
      }
    } else {
      // This is an update to a thread that we already know about
      // TODO: is there a better way to check for these types of messages.
      final ConversationThread conversationThread;
      synchronized (this) {
        conversationThread = conversations.get(ts);
        if (conversationThread != null) {
          return;
        }
      }
    }

    if (thread_ts == null || thread_ts.isEmpty() || thread_ts.equals(ts)) {
      startConversation(ts, session, event);
    } else {
      // we are already on a thread.
      // however, it is not a known conversation thread.
      handleIllegalNestedConversation(session, event);
    }
  }

  /**
   *
   * @param session
   * @param event
   * @throws IOException 
   */
  @Controller(events = EventType.MESSAGE)
  public void onReceiveMessage(WebSocketSession session, Event event) throws IOException {
    final String ts = event.getTs();
    final String thread_ts = event.getThreadTs();

    if (thread_ts != null) {
      final ConversationThread conversationThread;
      synchronized (this) {
        conversationThread = conversations.get(thread_ts);
      }
      if (conversationThread != null) {
        assert(!thread_ts.equals(ts));
        continueConversation(conversationThread, session, event);
        return;
      }
    }
    // otherwise, this is not part of a thread
  }

  private void handleIllegalNestedConversation(WebSocketSession session,
      Event event) {
    final Message message = new Message("I am sorry"
        + ".  I would like to help you, but we need a dedicated thread to do so :grinning:."
        + "Please send me a direct message or mention me in a top-level (non-threaded)"
        + "message.\nIf you are getting this error in the middle of a conversation with me,"
        + "then I must have forgotten that we were talking.  Sometimes I can be a bit forgetful :sleeping:.");
    message.setThreadTs(event.getThreadTs());
    reply(session, event, message);
  }

  private void startConversation(String thread_ts, WebSocketSession session,
      Event event) throws IOException {

    final String subtype = event.getSubtype();
    if (subtype != null && subtype.equals("message_replied")) {
      return;
    }
    final String messageUser = event.getUserId();
    if (messageUser == null
        || messageUser.equals(slackService.getCurrentUser().getId())) {
      return;
    }

    ConversationThread conversationThread = null;
    conversationThread = new ConversationThread(thread_ts, new RulebotConversation(this.credFile, this.wsConfig));

    final Message message = new Message("Hi"
        + ", @rulebot is here to help you.");
    message.setThreadTs(thread_ts);
    reply(session, event, message);

    synchronized (this) {
      conversations.put(thread_ts, conversationThread);
    }
    conversationThread.sendUserInput(session, event, event.getText());
  }

  private void continueConversation(ConversationThread conversationThread,
      WebSocketSession session, Event event) throws IOException {
    conversationThread.sendUserInput(session, event, event.getText());
  }

}
