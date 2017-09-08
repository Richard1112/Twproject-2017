/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 *
 * Redistributed under the terms of the BSD license from:
 * jwma Java WebMail
 * Copyright (c) 2002 Dieter Wimberger et al.
 *
 * jwma is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.util;

import java.io.DataInputStream;
import java.net.URL;
import java.rmi.server.UID;
import java.security.SecureRandom;

/**
 * Utility class exposing a method that will return
 * a unique identifier.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class UIDGenerator {

  //class attributes
  private static SecureRandom c_Random;
  private static SecureRandom c_SeedRandom;
  private static int c_ReseedCounter = 0;
  private static boolean c_SeedWithHotBits = false;

  //create seeded random
  static {
    c_Random = new SecureRandom();
    c_SeedRandom = new SecureRandom();
    c_SeedWithHotBits = new Boolean(
        System.getProperty("jpim.uidgen.hotbits", "false")).booleanValue();
    seedRandom();
  }

  /**
   * Returns a UID (unique identifier) as <tt>String</tt>.
   * The identifier represents the MD5 hashed combination
   * of a <tt>java.rmi.server.UID</tt> instance, a random padding of
   * <tt>RANDOM_PADDING</tt> length, it's identity hashcode and
   * <tt>System.currentTimeMillis()</tt>.
   *
   * @return the UID as <tt>String</tt>.
   */
  public static final synchronized String getUID() {
    byte[] buffer = new byte[RANDOM_PADDING];
    String u = new UID().toString();
    int i = System.identityHashCode(u);
    long d = System.currentTimeMillis();
    //create random padding
    c_Random.nextBytes(buffer);
    u = u + new String(buffer);

    if (c_ReseedCounter == RANDOM_RESEED) {
      seedRandom();
      c_ReseedCounter = 0;
    } else {
      c_ReseedCounter++;
    }
    return MD5.hash(u + i + d);
  }//getUID

  /**
   * If the <tt>HotBits</tt> Server is available, <tt>Random</tt>
   * will be seeded with a real random long.
   * <p>
   * <a href="http://www.fourmilab.ch/hotbits/">HotBits</a> is located
   * at Fermilab, Switzerland.
   *
   */
  public static final void seedRandom() {
    if(c_SeedWithHotBits) {
      try {
        URL url = new URL(HOTBITS_URL);
        DataInputStream din = new DataInputStream(url.openStream());
        c_Random.setSeed(din.readLong());
        din.close();
      } catch (Exception ex) {
        //use what is available
        c_Random.setSeed(c_SeedRandom.getSeed(8));
      }
    } else {
      c_Random.setSeed(c_SeedRandom.getSeed(8));
    }
  }//seedRandom

  public static final void main(String[] args) {
    int i = 0;
    System.out.println("Seed with Hotbits = " + c_SeedWithHotBits);
    long start = System.currentTimeMillis();
    while (i < 1000) {
      System.out.println(getUID());
      i++;
    }
    long stop = System.currentTimeMillis();
    System.out.println("Time =" + (stop - start) + "[ms]");
  }//main

  public static final int RANDOM_PADDING = 256;
  public static final int RANDOM_SEED_LENGTH = 6;
  public static final int RANDOM_RESEED = 1000;

  public static final String HOTBITS_URL =
      "http://www.fourmilab.ch/cgi-bin/uncgi/Hotbits?nbytes=" +
      RANDOM_SEED_LENGTH +
      "&fmt=bin";

}//UIDGenerator
