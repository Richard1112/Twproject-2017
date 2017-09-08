package com.twproject.meeting;

import org.hibernate.search.annotations.DocumentId;
import org.hibernate.annotations.Type;
import org.jblooming.ontology.LookupStringSupport;

import javax.persistence.*;
import java.io.Serializable;

/**
 * @author Roberto Baldi rbaldi@open-lab.com
 *         Date: 3-ago-2007 : 15.50.37
 */
@Entity
@Table(name = "twk_discPointType")
public class DiscussionPointType extends LookupStringSupport {

  private String code;
  private String name;

  public DiscussionPointType() {
    super();
  }

  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  public Serializable getId() {
      return super.getId();
  }
  
  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }
}
