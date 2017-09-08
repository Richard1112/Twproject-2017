package org.jblooming.messaging;

import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.operator.Operator;

import java.util.Date;

public class SomethingHappened extends IdentifiableSupport {

  /**
   * link to page; may be necessary
   */
  private SerializedMap<String, String> messageParams = new SerializedMap();
  private String theClass;
  private String identifiableId;
  private String eventType;
  private Date happenedAt = new Date();
  private Date happeningExpiryDate;
  private String messageTemplate;
  private String link;
  private Operator whoCausedTheEvent;

  public SomethingHappened() {
  }

  public SomethingHappened(String eventType, String eventObject, String command, Date happenedAt, Date happeningExpiryDate/*, String whatsUp*/) {
    this.theClass = eventType;
    this.identifiableId = eventObject;
    this.eventType = command;
    this.happenedAt = happenedAt;
    this.happeningExpiryDate = happeningExpiryDate;
//    this.whatsUp = whatsUp;
  }

  public SerializedMap<String, String> getMessageParams() {
    return messageParams;
  }

  public void setMessageParams(SerializedMap<String, String> messageParams) {
    this.messageParams = messageParams;
  }

  public String getTheClass() {
    return theClass;
  }

  public void setTheClass(String theClass) {
    this.theClass = theClass;
  }

  public String getIdentifiableId() {
    return identifiableId;
  }

  public void setIdentifiableId(String identifiableId) {
    this.identifiableId = identifiableId;
  }

  public String getEventType() {
    return eventType;
  }

  public void setEventType(String eventType) {
    this.eventType = eventType;
  }

  public Date getHappenedAt() {
    return happenedAt;
  }

  public void setHappenedAt(Date happenedAt) {
    this.happenedAt = happenedAt;
  }

  public Date getHappeningExpiryDate() {
    return happeningExpiryDate;
  }

  public void setHappeningExpiryDate(Date happeningExpiryDate) {
    this.happeningExpiryDate = happeningExpiryDate;
  }

  public String getMessageTemplate() {
    return messageTemplate;
  }

  public void setMessageTemplate(String messageTemplate) {
    this.messageTemplate = messageTemplate;
  }

  public void setIdentifiable(Identifiable identifiable) {
     setTheClass(ReflectionUtilities.deProxy(identifiable.getClass().getName()));
     setIdentifiableId(identifiable.getId().toString());
  }

  public String getLink() {
    return link;
  }

  public void setLink(String link) {
    this.link = link;
  }

  public Operator getWhoCausedTheEvent() {
    return whoCausedTheEvent;
  }

  public void setWhoCausedTheEvent(Operator whoCausedTheEvent) {
    this.whoCausedTheEvent = whoCausedTheEvent;
  }
}
