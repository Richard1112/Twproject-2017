package org.jblooming.ontology;

import org.hibernate.annotations.Type;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;

import javax.persistence.*;
import java.io.Serializable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */

@Entity
@Table(name = "_persistentText")
public class  PersistentText extends IdentifiableSupport {

  private String text;

  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  public Serializable getId() {
      return super.getId();
  }

  @Lob
  @Type(type = "org.hibernate.type.TextType")
  public String getText() {
    return text;
  }

  public void setText(String text) {
    this.text = text;
  }

  public static PersistentText load(Serializable id) throws FindByPrimaryKeyException {
    return (PersistentText) PersistenceHome.findByPrimaryKey(PersistentText.class, id);
  }


}
