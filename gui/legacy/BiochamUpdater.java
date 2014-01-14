/* BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
 * Copyright 2003-2008, INRIA, Projet Contraintes
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * BiochamUpdater.java
 * by Sylvain Soliman */

package fr.inria.contraintes.biocham;

import java.io.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.DefaultHandler;
import javax.swing.*;

public class BiochamUpdater extends Thread {
   int state = 0;
   String uri;
   BiochamGUI bc;

   BiochamUpdater(String uri, BiochamGUI bc) {
      state = 0;
      this.uri=uri;
      this.bc=bc;
   }

   public void run() {
      try {
         SAXParser saxParser = SAXParserFactory.newInstance().newSAXParser();
         saxParser.parse(uri, (DefaultHandler) new DefaultHandler() {
            public void startElement(String uri, String localName, String qName,
               Attributes attrs) {
               if (qName.equals("item")) {
                  state++;
               } else if (qName.equals("title")) {
                  state++;
               }
            }

            public void endElement(String uri, String localName, String qName) {
               if (state==3) {
                  state++;
               }
            }
            public void characters(char[] buf, int offset, int len) {
               if (state==3) {
                  String version=new String(buf, offset, len).substring(8);
                  if (Float.parseFloat(version) > Float.parseFloat("3.5") &&
                     JOptionPane.showConfirmDialog(bc.frame,
                        "Version "+version+" of BIOCHAM seems to be available, do you want to download it ?",
                     "Confirmation",
                     JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
                     bc.browseUrl("http://contraintes.inria.fr/BIOCHAM/"); 
                     }
               }
            }
         });
      } catch (Throwable t) {
      }
   }

   public static void main(String[] argv) {
      BiochamUpdater bu = new BiochamUpdater(argv[0], null);
      bu.start();
   }
}
