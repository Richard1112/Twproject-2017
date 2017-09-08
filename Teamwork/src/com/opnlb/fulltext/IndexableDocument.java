package com.opnlb.fulltext;

import org.jblooming.ontology.Identifiable;

  public interface IndexableDocument extends Identifiable {

  String getKeywords();

  String getContent();

  String getCode();

  String getSumma();

  String getAuthor();

  String getKind();

  int getAreaId();

  String getMimeType();

  String getUrl();

  int getType();
}
