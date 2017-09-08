package com.teamwork.expand;

import org.jblooming.remoteFile.FileStorage;

public class ReportDocument extends FileStorage{

	
	public static ReportDocument instance=null;
	
	private ReportDocument(){};
	
	public static synchronized ReportDocument getInstance(){
		if(instance==null){
			return new ReportDocument();
		}
		return instance;
	}
	
}
