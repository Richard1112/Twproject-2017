package com.teamwork.expand;

import org.jblooming.remoteFile.Document;
import org.jblooming.waf.html.display.Explorer;

public class ReportExplorer extends Explorer{

	public ReportExplorer(Class aClass, Document doc) {
		super(aClass, doc);
	    this.urlToInclude = "/applications/teamwork/task/report/partExplorer.jsp";
	}

}
