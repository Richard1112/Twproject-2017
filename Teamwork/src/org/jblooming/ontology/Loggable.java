package org.jblooming.ontology;

import java.util.Date;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 22-lug-2005 : 10.07.39
 */
public interface Loggable {
  Date getLastModified();

  void setLastModified(Date lastModified);

  String getLastModifier();

  void setLastModifier(String lastModifier);

  String getCreator();

  void setCreator(String creator);

  Date getCreationDate();

  void setCreationDate(Date creationDate);
}
