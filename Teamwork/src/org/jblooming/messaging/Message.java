package org.jblooming.messaging;

import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.Application;
import org.jblooming.security.Group;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.ClientEntry;

import javax.persistence.Transient;
import java.util.*;
import java.io.Serializable;

public class Message extends IdentifiableSupport {

  private String subject;
  private String messageBody;
  private Operator fromOperator;
  private Operator toOperator;
  private String media;
  private Date lastTry;
  private String status;
  private int numberOfTries;
  private Date expires;
  private Date received;
  private Date readOn;
  private String lastError;
  private String link;
  private int groupMessageId;

  public Message() {}

  public Message(String subject, String messageBody, Operator toOperator, String media) {

    this.subject = subject;
    this.messageBody = messageBody;
    this.toOperator = toOperator;
    this.media = media;
  }

  public String getSubject() {
    return subject;
  }

  public void setSubject(String subject) {
    this.subject = subject;
  }

  public String getMessageBody() {
    return messageBody;
  }

  public void setMessageBody(String messageBody) {
    this.messageBody = messageBody;
  }

  public Operator getFromOperator() {
    return fromOperator;
  }

  public void setFromOperator(Operator fromOperator) {
    this.fromOperator = fromOperator;
  }

  public Operator getToOperator() {
    return toOperator;
  }

  public void setToOperator(Operator toOperator) {
    this.toOperator = toOperator;
  }


  public String getMedia() {
    return media;
  }

  public void setMedia(String media) {
    this.media = media;
  }

  public Date getLastTry() {
    return lastTry;
  }

  public void setLastTry(Date lastTry) {
    this.lastTry = lastTry;
  }

  public String getStatus() {
    return status;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  public int getNumberOfTries() {
    return numberOfTries;
  }

  public void setNumberOfTries(int numberOfTries) {
    this.numberOfTries = numberOfTries;
  }

  public Date getExpires() {
    return expires;
  }

  public void setExpires(Date expires) {
    this.expires = expires;
  }
  @Transient
  public void setDefaultExpires() {
    int days=new ClientEntry("dummy",I18n.get("CUSTOM_FEATURE_MESSAGES_EXPIRES_DAYS")).intValueNoErrorCodeNoExc();
    days=days<=0?15:days;
    setExpires(new Date(System.currentTimeMillis() + (CompanyCalendar.MILLIS_IN_DAY*days)));
  }


  public Date getReceived() {
    return received;
  }

  public void setReceived(Date received) {
    this.received = received;
  }

  public String getLastError() {
    return lastError;
  }

  public void setLastError(String lastError) {
    this.lastError = lastError;
  }

  public String getLink() {
    return link;
  }

  public void setLink(String link) {
    this.link = link;
  }


  public int getGroupMessageId() {
    return groupMessageId;
  }

  public void setGroupMessageId(int groupMessageId) {
    this.groupMessageId = groupMessageId;
  }

  public Date getReadOn() {
    return readOn;
  }

  public void setReadOn(Date readOn) {
    this.readOn = readOn;
  }
}
