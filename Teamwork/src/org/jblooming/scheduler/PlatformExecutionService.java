package org.jblooming.scheduler;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Mar 5, 2007
 * Time: 9:33:23 AM
 */
public class PlatformExecutionService {

  public static ExecutorService executorService = Executors.newCachedThreadPool();

}
