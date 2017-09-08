package org.jblooming.ontology;

import org.jblooming.remoteFile.Document;
import org.jblooming.security.Securable;

import java.util.Set;

public interface Documentable extends PerformantNode, Securable {

  Set<Document> getDocuments();

}
