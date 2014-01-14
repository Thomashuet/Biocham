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
 * ParamTable.java
 * by Sylvain Soliman */

package fr.inria.contraintes.biocham;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.text.*;

public class ParamTable implements MouseListener {
   Vector<Param> table;
   BiochamGUI biocham;
   JPanel panel;

   static final int LEFT_OFF = 5, TOP_OFF = 5,
      HEIGHT = 25, RIGHT_OFF = 5, MIDDLE = 10;
   static final Color VERY_LIGHT_PINK = new Color(255, 230, 230);

   
   ParamTable(BiochamGUI b, JPanel p) {
      table = new Vector<Param> ();
      biocham = b;
      panel = p;
   }

   public class Param {
      private String name;
      private String value, origValue;

      Param(String s, String k) {
         name = s;
         origValue = new String(k);
         value = origValue;
      }

      void setValue(String k) {
         value = new String(k);
      }
      
      boolean hasChanged() {
         return (! value.equals(origValue));
      }

      String getName() {
         return name;
      }
      
      String getValue() {
         return value;
      }

      void resetValue() {
         setValue(origValue);
      }
   }

   int indexOf(String s) {
      int i=0;
      while (i<table.size() && !getName(i).equals(s))
         i++;
      if (i == table.size())
         return -1;
      return i;
   }

   String getName(int i) {
      return table.get(i).getName();
   }
   
   String getValue(int i) {
      return table.get(i).getValue();
   }
   
   void reset () {
      table.removeAllElements();
      panel.removeAll();
      panel.setLayout(new SpringLayout());
      panel.setPreferredSize(new Dimension(0,HEIGHT));
      panel.getParent().validate();
      panel.getParent().repaint();
   }
   
   void setValue(String s, String v) {
      int i = indexOf(s);
      if (i<0) {
         table.add(new Param(s,v));

         i = table.size()-1;

         Spring maxSpring = Spring.constant(MIDDLE);
         JLabel l = new JLabel(s);
         l.addMouseListener(this);
         panel.add(l);
         
         SpringLayout layout = (SpringLayout) panel.getLayout();

         /* NumberFormat format = NumberFormat.getNumberInstance(Locale.US);
         format.setMaximumFractionDigits(12);

         JFormattedTextField field = new JFormattedTextField(format);*/

         JFormattedTextField field = new JFormattedTextField();
         
         field.setValue(v);
         

         field.setHorizontalAlignment(JTextField.RIGHT);
         l.setLabelFor(field);

         field.addPropertyChangeListener("value", biocham);
         panel.add(field);

         layout.putConstraint(SpringLayout.WEST, l, LEFT_OFF,
               SpringLayout.WEST, panel);

         layout.putConstraint(SpringLayout.NORTH, l, TOP_OFF+HEIGHT*i,
               SpringLayout.NORTH, panel);

         layout.putConstraint(SpringLayout.NORTH, field, TOP_OFF+HEIGHT*i,
               SpringLayout.NORTH, panel);

         layout.putConstraint(SpringLayout.EAST, field, -RIGHT_OFF,
               SpringLayout.EAST, panel);
         
         for (i=0;i<table.size();++i)
            maxSpring = Spring.max(maxSpring,
                  Spring.sum(
                     layout.getConstraints(panel.getComponent(2*i)).getWidth(),
                     Spring.constant(10)));

         for (i=0;i<table.size();++i)
            layout.putConstraint(SpringLayout.WEST, panel.getComponent(2*i+1),
                  maxSpring,
                  SpringLayout.WEST, panel);

         panel.setPreferredSize(new Dimension(0,HEIGHT*(table.size()+1)));
         
         // Update the parent JScrollPane
         panel.getParent().validate();
      }
      else {
         Param pcur = table.get(i);
         pcur.setValue(v);
         JFormattedTextField tf = ((JFormattedTextField) panel.getComponent(2*i+1));
         tf.removePropertyChangeListener("value", biocham);
         tf.setValue(v);

         // Write in red if a value has changed
         if (pcur.hasChanged())
            //tf.setForeground(Color.RED);
            tf.setBackground(VERY_LIGHT_PINK);
         else
            //tf.setForeground(Color.BLACK);
            tf.setBackground(Color.WHITE);
         
         tf.addPropertyChangeListener("value", biocham);
      }
   }

   
   void resetValue(String s) {
      int i = indexOf(s);
      if (i>=0) {
         Param pcur = table.get(i);
         pcur.resetValue();
         JFormattedTextField tf = ((JFormattedTextField) panel.getComponent(2*i+1));
         tf.setValue(pcur.getValue());
         //tf.setForeground(Color.BLACK);
         tf.setBackground(Color.WHITE);
      }
   }
   
   int size() {
      return table.size();
   }
   
   // MouseListener methods

   public void mouseClicked(MouseEvent e) {}

   public void mousePressed(MouseEvent e) {
      if (e.getClickCount() == 2) {
         resetValue(((JLabel) e.getComponent()).getText());
      }
   }

   public void mouseReleased(MouseEvent e) {}

   public void mouseEntered(MouseEvent e) {}

   public void mouseExited(MouseEvent e) {}
}
