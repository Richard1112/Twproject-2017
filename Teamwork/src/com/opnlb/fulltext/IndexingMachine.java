package com.opnlb.fulltext;

import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.PersistentFile;

import java.util.*;
import java.io.Serializable;

/**
 * Written by
 * Roberto Bicchierai rbicchierai@open-lab.com
 * Pietro Polsinelli ppolsinelli@open-lab.com
 * for the Twproject Project Management application - http://twproject.com
 */
public class IndexingMachine extends TimerTask {

  public static IndexingMachine machine = new IndexingMachine();
  public long tick = 10000;
  private boolean stopped = true;
  private boolean indexing = false;

  protected static List<DataForLucene> toBeExecuteds = new ArrayList();

  protected IndexingMachine() {
  }

  public static void start() {

    machine.stopped = false;

    if (!machine.indexing) {
      machine.run();
    }
  }

  public static void stop() {
    machine.stopped = true;
  }

  public void run() {

    if (toBeExecuteds!=null && toBeExecuteds.size() > 0) {
      DataForLucene ij = toBeExecuteds.get(0);
      synchronized (toBeExecuteds) {
        toBeExecuteds.remove(0);
      }
      indexing = true;
      ij.indexMe();
      indexing = false;
    }

    if (toBeExecuteds!=null && toBeExecuteds.size() > 0)
      tick = 20;
    else
      tick = 10000;

    if (machine!=null && !machine.stopped && !machine.indexing) {
      Timer t = new Timer(false);
      machine = new IndexingMachine();
      machine.stopped = false;
      t.schedule(machine, tick);
    }
  }

  public static void addToBeIndexed(Identifiable i, Serializable areaId, PersistentFile pf) {
    DataForLucene ij = new DataForLucene();
    ij.clazz = i.getClass();
    ij.id = i.getId();
    ij.areaid = areaId;
    ij.pf = pf;
    if (!toBeExecuteds.contains(ij))
      synchronized (toBeExecuteds) {
        toBeExecuteds.add(ij);
      }
  }

  public static int getQueueSize() {
    return toBeExecuteds.size();
  }

  public static boolean isRunning() {
    return !machine.stopped;
  }

  public static boolean isIndexing() {
    return machine.indexing;
  }

}
