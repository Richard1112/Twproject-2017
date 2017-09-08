package org.jblooming.ontology;

import net.sf.json.JSONObject;

import java.io.Serializable;


public interface Identifiable  {
  /**
   * @deprecated
   */
  public int getIntId();

  public Serializable getId();

  public void setId(Serializable id);

  public String getName();

  public String getDisplayName();

  public JSONObject jsonify();
}
