package org.jblooming.waf.settings;

import org.hibernate.annotations.Type;
import org.jblooming.ontology.IdentifiableSupport;

import javax.persistence.*;
import java.io.Serializable;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 8-set-2008 : 13.05.05
 */
@Entity
@Table(name = "_i18nEntry")
public class I18nEntryPersistent extends IdentifiableSupport {

  private String code;
  private String application;
  private String language;
  private String value;

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

  public String getApplication() {
    return application;
  }

  public void setApplication(String application) {
    this.application = application;
  }

  public String getLanguage() {
    return language;
  }

  public void setLanguage(String language) {
    this.language = language;
  }

@Column (length = 1000)
public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = value;
  }
}
