package com.twproject.worklog.businessLogic;

import java.util.Date;

/**
 * Created by Silvia The Great
 * (c) Open Lab ab urbe condita - today.
 */
public class WorklogToImport {
  private String id;
  private Date creationDate;
  private String message;
  private String details;

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public Date getCreationDate() {
    return creationDate;
  }

  public void setCreationDate(Date creationDate) {
    this.creationDate = creationDate;
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }

  public String getDetails() {
    return details;
  }

  public void setDetails(String details) {
    this.details = details;
  }
}
