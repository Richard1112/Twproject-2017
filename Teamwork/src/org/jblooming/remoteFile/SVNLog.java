package org.jblooming.remoteFile;

import java.util.Date;

/**
 * Created by Silvia The Great
 * (c) Open Lab ab urbe condita - today.
 */
public class SVNLog {

  private long  logId;
  private String logValue;
  private Date logDate = new Date();
  private String logMessage;

  public void setLogId(long id){
    logId =id;
  }

  public void setLog(String log){
    logValue = log;
  }

  public String getLogValue(){
     return logValue;
  }

  public long getLogId(){
     return logId;
  }

 public void setDate(Date date){
    logDate = date;
  }

  public Date getLogdate(){
     return logDate;
  }

   public void setLogMessage(String logMegg){
    logMessage = logMegg;
  }

  public String getLogMessage(){
     return logMessage;
  }
}