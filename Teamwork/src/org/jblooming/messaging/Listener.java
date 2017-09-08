package org.jblooming.messaging;

import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.operator.Operator;
import org.jblooming.security.Group;
import org.jblooming.utilities.ReflectionUtilities;

import java.util.Date;

public class Listener extends IdentifiableSupport {

  /**
   * what is the listener listening to ? e.g. com.twproject.task.Task
   */
  private String theClass;
  private String identifiableId;

  /**
   * update, insert, add worklog
   */
  private String eventType;
  private Date validityStart;
  private Date validityEnd;
  private String media;
  private Operator owner;

  /**
   * may contain tag <linkParam /> that gets substituted with events
   */
  private String body;

  private SerializedMap<String, String> additionalParams = new SerializedMap();

  private boolean oneShot;

  private boolean listenDescendants = false;

  private Date lastMatchingDate;


  public Listener() {
  }

  public Listener(Operator owner) {
    this.owner = owner;
  }

  public Listener(String theClass, String identifiableId, String command, Date validityStart, Date validityEnd,
                  String media, Operator owner, String body, SerializedMap<String, String> additionalParams, boolean oneShot) {
    this.theClass = ReflectionUtilities.deProxy(theClass);
    this.identifiableId = identifiableId;
    this.eventType = command;
    this.validityStart = validityStart;
    this.validityEnd = validityEnd;
    this.media = media;
    this.owner = owner;
    this.body = body;
    this.additionalParams = additionalParams;
    this.oneShot = oneShot;
  }


  public String getTheClass() {
    return theClass;
  }

  public void setTheClass(String theClass) {
    this.theClass = ReflectionUtilities.deProxy(theClass);
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

  public Date getValidityStart() {
    return validityStart;
  }

  public void setValidityStart(Date validityStart) {
    this.validityStart = validityStart;
  }

  public Date getValidityEnd() {
    return validityEnd;
  }

  public void setValidityEnd(Date validityEnd) {
    this.validityEnd = validityEnd;
  }

  public String getMedia() {
    return media;
  }

  public void setMedia(String media) {
    this.media = media;
  }

  public Operator getOwner() {
    return owner;
  }

  public void setOwner(Operator owner) {
    this.owner = owner;
  }

  public String getBody() {
    return body;
  }

  public void setBody(String body) {
    this.body = body;
  }

  public SerializedMap<String, String> getAdditionalParams() {
    return additionalParams;
  }

  public void setAdditionalParams(SerializedMap<String, String> additionalParams) {
    this.additionalParams = additionalParams;
  }

  public boolean isOneShot() {
    return oneShot;
  }

  public void setOneShot(boolean oneShot) {
    this.oneShot = oneShot;
  }

  public void setIdentifiable(Identifiable identifiable) {
     setTheClass(ReflectionUtilities.deProxy(identifiable.getClass().getName()));
     setIdentifiableId(identifiable.getId().toString());
  }

  public boolean isListenDescendants() {
    return listenDescendants;
  }

  public void setListenDescendants(boolean listenDescendants) {
    this.listenDescendants = listenDescendants;
  }

  public Date getLastMatchingDate() {
    return lastMatchingDate;
  }

  public void setLastMatchingDate(Date lastMatchingDate) {
    this.lastMatchingDate = lastMatchingDate;
  }

}
