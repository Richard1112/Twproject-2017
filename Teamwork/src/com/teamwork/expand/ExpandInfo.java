package com.teamwork.expand;

import org.jblooming.logging.Auditable;
import org.jblooming.ontology.LoggableIdentifiableSupport;

public class ExpandInfo extends LoggableIdentifiableSupport implements Comparable, Auditable{
	  
	protected String name;
	
	
	 public ExpandInfo() {
	  }


	public String getName() {
		return name;
	}


	public void setName(String name) {
		this.name = name;
	}




	 
}
