package com.twproject.worklog;

import net.sf.json.JSONObject;
import org.jblooming.ontology.LookupIntSupport;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;

public class WorklogStatus extends LookupIntSupport {


  private String color="#";

  public String getName(){
    return getIntValue()+" - "+getDescription();
  }

  public String getColor() {
    return color;
  }

  public void setColor(String color) {
    this.color = color;
  }

  public static WorklogStatus load(String id) throws FindByPrimaryKeyException {
    return (WorklogStatus) PersistenceHome.findByPrimaryKey(WorklogStatus.class,id);
  }

  public JSONObject jsonify() {
    JSONObject jso=super.jsonify();
    jso.element("id",getId());
    jso.element("color",getColor());
    jso.element("description",getDescription());
    return jso;
  }
}

