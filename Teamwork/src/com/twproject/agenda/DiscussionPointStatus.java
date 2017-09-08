package com.twproject.agenda;

import org.hibernate.annotations.Type;
import org.jblooming.ontology.LookupSupport;

import javax.persistence.*;
import java.io.Serializable;

@Entity
@Table(name = "twk_discPointStatus")
public class DiscussionPointStatus extends LookupSupport {

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  @Type(type = "int")
  public Serializable getId() {
    return super.getId();
  }


}
