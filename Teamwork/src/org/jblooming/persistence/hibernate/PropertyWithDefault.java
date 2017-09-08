package org.jblooming.persistence.hibernate;

import org.jblooming.scheduler.Executable;

import java.util.List;
import java.util.ArrayList;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Jul 17, 2008
 * Time: 2:36:40 PM
 */
 public class PropertyWithDefault {

    //in order to accept both classes and entity names
    public Class clazz;
    public String propertyName;
    public Object defaultValue;


    //computed:
    public boolean needsToBeUpdated = false;
    public String table;
    public String column;

    public int priority=0;


  }