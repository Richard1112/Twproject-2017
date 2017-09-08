package org.jblooming.messaging;

import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.operator.Operator;

import java.util.List;
import java.util.ArrayList;
import java.util.Date;
import java.io.Serializable;


public class ChatUtilities {

  /**
   * @param chatWith        is the operator id to chat with. If null the public thread is given
   * @param loggedId
   * @param howManyMessages
   * @return the list with the last "howManyMessages" message thread. The oldest one is the first.
   */
  public static List<ChatMessage> getChatThread(Serializable chatWith, Serializable loggedId, int howManyMessages) {
    List<ChatMessage> messageList = (List) ApplicationState.applicationParameters.get("MESSAGELIST");
    List<ChatMessage> list = null;
    if (messageList != null && messageList.size()>0) {
      List<ChatMessage> filteredMessageList = new ArrayList<ChatMessage>();
      for (int j = 0; j < messageList.size(); j++) {
        ChatMessage msg = messageList.get(j);
        if (chatWith != null) { // test if filtering is active get only messages  logged->chatWith & chatWith->logged
          if (msg != null && msg.fromID != null && msg.toID != null &&
                  ((msg.fromID.equals(chatWith) && msg.toID.equals(loggedId)) ||
                          (msg.toID.equals(chatWith) && msg.fromID.equals(loggedId)))
          ) {
            filteredMessageList.add(msg);
          }
        } else { // chatWith for public or to logged
          if (msg != null && (msg.toID == null || loggedId.equals(msg.toID))) {
            filteredMessageList.add(msg);
          }
        }
        if (filteredMessageList.size() >= howManyMessages)
          break;
      }
      list = filteredMessageList;
    }
    return list;
  }

  public static ChatMessage getLastUserMessage(Serializable id) {
    List<ChatMessage> msgs =getChatThread(null,id,1);
    if (msgs!=null&&msgs.size()>0)
      return msgs.get(0);
    else
      return null;
  }

  public static String getHtmlTagForMessage(ChatMessage msg, Serializable loggedId) {
    boolean isToMe = (msg.toID != null && msg.toID.equals(loggedId));
    boolean isFromMe = (msg.fromID != null && msg.fromID.equals(loggedId));

    String tag;
    if (isToMe)
      tag = "B";
    else if (isFromMe)
      tag = "I";
    else
      tag = null;

    return tag;
  }

  public static class ChatMessage {
    public String messageBody;
    public String fromID;
    public String from;
    public String toID;
    public String to;    
    public int numberOfTries;
    public Date received;


  }

}
