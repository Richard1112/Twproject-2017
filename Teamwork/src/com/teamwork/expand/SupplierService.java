package com.teamwork.expand;

import java.util.Date;

import org.jblooming.logging.Auditable;
import org.jblooming.ontology.LoggableIdentifiableSupport;

public class SupplierService extends LoggableIdentifiableSupport implements Comparable, Auditable{
	
	
	private String serviceId;
	  
    private String description;
    
    private String serviceTypeId;
    
    private String resourceId;
    private Date serviceDate;
    
    private String totalNum;
    

	public String getServiceId() {
		return serviceId;
	}

	public void setServiceId(String serviceId) {
		this.serviceId = serviceId;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getResourceId() {
		return resourceId;
	}

	public void setResourceId(String resourceId) {
		this.resourceId = resourceId;
	}

	public String getServiceTypeId() {
		return serviceTypeId;
	}

	public void setServiceTypeId(String serviceTypeId) {
		this.serviceTypeId = serviceTypeId;
	}



	public Date getServiceDate() {
		return serviceDate;
	}

	public void setServiceDate(Date serviceDate) {
		this.serviceDate = serviceDate;
	}

	public String getTotalNum() {
		return totalNum;
	}

	public void setTotalNum(String totalNum) {
		this.totalNum = totalNum;
	}


    
}
