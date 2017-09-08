package com.twproject.security;

import org.hibernate.Query;
import org.jblooming.ontology.LoggableIdentifiable;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.security.Area;
import org.jblooming.security.Role;

import java.io.Serializable;
import java.util.Date;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Mar 7, 2007
 * Time: 5:35:57 PM
 *
 */

public class TeamworkArea extends Area implements LoggableIdentifiable {

  protected Date lastModified;
  protected String lastModifier;
  protected String creator;
  protected Date creationDate;

  private int enabledOperators;
  private String freeAccount;
  private Date expiry;
  private Date lastLoginOnArea;
  private Date beyondFreeVersion;

  public void setFreeAccount(String freeAccount) {
    this.freeAccount = freeAccount;
  }

  public String getFreeAccount() {
    return freeAccount;
  }

  public Date getExpiry() {
    return expiry;
  }

  public void setExpiry(Date expiry) {
    this.expiry = expiry;
  }

  public int getEnabledOperators() {
    return enabledOperators;
  }

  public void setEnabledOperators(int enabledOperators) {
    this.enabledOperators = enabledOperators;
  }

  public Date getLastLoginOnArea() {
    return lastLoginOnArea;
  }

  public void setLastLoginOnArea(Date lastLoginOnArea) {
    this.lastLoginOnArea = lastLoginOnArea;
  }

  public Date getBeyondFreeVersion() {
    return beyondFreeVersion;
  }

  public void setBeyondFreeVersion(Date beyondFreeVersion) {
    this.beyondFreeVersion = beyondFreeVersion;
  }

  public Date getCreationDate() {
    return creationDate;
  }

  public void setCreationDate(Date creationDate) {
    this.creationDate = creationDate;
  }

  public Date getLastModified() {
    return lastModified;
  }

  public void setLastModified(Date lastModified) {
    this.lastModified = lastModified;
  }

  public String getLastModifier() {
    return lastModifier;
  }

  public void setLastModifier(String lastModifier) {
    this.lastModifier = lastModifier;
  }

  public String getCreator() {
    return creator;
  }

  public void setCreator(String creator) {
    this.creator = creator;
  }

  public Role getOperationalRole() {
    OqlQuery oql = new OqlQuery("select role from " + Role.class.getName() + " as role where lower(role.name)=:roleName and role.area=:area");
    Query query = oql.getQuery();
    query.setParameter("roleName", "operational");
    query.setEntity("area", this);
    return (Role) oql.uniqueResultNullIfEmpty();
  }

  public static TeamworkArea load(Serializable id) throws FindByPrimaryKeyException {
    return (TeamworkArea) PersistenceHome.findByPrimaryKey(TeamworkArea.class,id);
  }
}
