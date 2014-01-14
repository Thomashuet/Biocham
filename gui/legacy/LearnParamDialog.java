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
 * LearnParamDialog.java
 * by Sylvain Soliman */

package fr.inria.contraintes.biocham;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class LearnParamDialog extends JDialog {

   /**
    * Command string for a cancel action (e.g., a button or menu item).
    * This string is never presented to the user and should
    * not be internationalized.
    */
   private String CMD_CANCEL = "cmd.cancel"/*NOI18N*/;

   /**
    * Command string for an "OK" action (e.g., a button or menu item).
    * This string is never presented to the user and should
    * not be internationalized.
    */
   private String CMD_OK = "cmd.ok"/*NOI18N*/;

   ParamTable pt;
   JTextField ltlText;
   JTextField timeText;
   JTextField stepsText;
   String result = null;
   JComboBox p1, p2, p3;
   JTextField t12, t13, t22, t23, t32, t33;

   public LearnParamDialog(JFrame parent, boolean modal,
         ParamTable p, float l) {
      super (parent, modal);
      pt = p;
      initComponents(l);
      pack();
   }

   private void initComponents(float l) {
      String[] list = new String[pt.size()+1];
      for (int i=0;i<pt.size();++i)
         list[i+1] = pt.getName(i);
      list[0] = "----";

      // initialize the window
      setTitle("Learn Parameters");
      Container contents = getContentPane();
      SpringLayout layout = new SpringLayout();
      contents.setLayout(layout);

      // Top
      JLabel ltlLabel = new JLabel("LTL specification: ");
      ltlText = new JTextField("", 20);
      contents.add(ltlLabel);
      contents.add(ltlText);
      layout.putConstraint(SpringLayout.WEST, ltlLabel, 10,
            SpringLayout.WEST, contents);
      layout.putConstraint(SpringLayout.NORTH, ltlLabel, 10,
            SpringLayout.NORTH, contents);
      layout.putConstraint(SpringLayout.WEST, ltlText, 5,
            SpringLayout.EAST, ltlLabel);
      layout.putConstraint(SpringLayout.NORTH, ltlText, 0,
            SpringLayout.NORTH, ltlLabel);

      JLabel timeLabel = new JLabel("Simulation time: ");
      timeText = new JTextField(l+"", 4);

      JLabel stepsLabel = new JLabel("Enumeration steps: ");
      stepsText = new JTextField("10", 3);
      contents.add(timeLabel);
      layout.putConstraint(SpringLayout.WEST, timeLabel, 10,
            SpringLayout.WEST, contents);
      layout.putConstraint(SpringLayout.NORTH, timeLabel, 10,
            SpringLayout.SOUTH, ltlLabel);
      contents.add(timeText);
      layout.putConstraint(SpringLayout.WEST, timeText, 5,
            SpringLayout.EAST, timeLabel);
      layout.putConstraint(SpringLayout.NORTH, timeText, 0,
            SpringLayout.NORTH, timeLabel);
      contents.add(stepsLabel);
      //layout.putConstraint(SpringLayout.WEST, stepsLabel, 30,
      //      SpringLayout.EAST, timeText);
      layout.putConstraint(SpringLayout.NORTH, stepsLabel, 0,
            SpringLayout.NORTH, timeLabel);
      contents.add(stepsText);
      layout.putConstraint(SpringLayout.EAST, stepsLabel, -5,
            SpringLayout.WEST, stepsText);
      layout.putConstraint(SpringLayout.EAST, stepsText, -10,
            SpringLayout.EAST, contents);
      layout.putConstraint(SpringLayout.NORTH, stepsText, 0,
            SpringLayout.NORTH, stepsLabel);

      // Middle
      JLabel h1 = new JLabel("Param");
      contents.add(h1);
      layout.putConstraint(SpringLayout.WEST, h1, 20,
            SpringLayout.WEST, contents);
      layout.putConstraint(SpringLayout.NORTH, h1, 20,
            SpringLayout.SOUTH, timeLabel);
      p1 = new JComboBox(list);
      p1.addActionListener(new ActionListener () {
         public void actionPerformed(ActionEvent e) {
            int i = p1.getSelectedIndex();
            if (i == 0) {
               t12.setText("");
               t13.setText("");
            } else {
               String s = pt.getValue(i-1)+"";
               t12.setText(s);
               t13.setText(s);
            }
         }
      });
      contents.add(p1);
      layout.putConstraint(SpringLayout.WEST, p1, 0,
            SpringLayout.WEST, h1);
      layout.putConstraint(SpringLayout.NORTH, p1, 20,
            SpringLayout.SOUTH, h1);

      JLabel h2 = new JLabel("From");
      contents.add(h2);
      layout.putConstraint(SpringLayout.WEST, h2, 40,
            SpringLayout.EAST, p1);
      layout.putConstraint(SpringLayout.NORTH, h2, 0,
            SpringLayout.NORTH, h1);
      
      t12 = new JTextField("",5);
      contents.add(t12);
      layout.putConstraint(SpringLayout.WEST, t12, 0,
            SpringLayout.WEST, h2);
      layout.putConstraint(SpringLayout.NORTH, t12, 0,
            SpringLayout.NORTH, p1);
      
      JLabel h3 = new JLabel("To");
      contents.add(h3);
      layout.putConstraint(SpringLayout.WEST, h3, 40,
            SpringLayout.EAST, t12);
      layout.putConstraint(SpringLayout.NORTH, h3, 0,
            SpringLayout.NORTH, h1);
      
      t13 = new JTextField("",5);
      contents.add(t13);
      layout.putConstraint(SpringLayout.WEST, t13, 0,
            SpringLayout.WEST, h3);
      layout.putConstraint(SpringLayout.NORTH, t13, 0,
            SpringLayout.NORTH, p1);
      
      p2 = new JComboBox(list);
      p2.addActionListener(new ActionListener () {
         public void actionPerformed(ActionEvent e) {
            int i = p2.getSelectedIndex();
            if (i == 0) {
               t22.setText("");
               t23.setText("");
            } else {
               String s = pt.getValue(i-1)+"";
               t22.setText(s);
               t23.setText(s);
            }
         }
      });
      contents.add(p2);
      layout.putConstraint(SpringLayout.WEST, p2, 0,
            SpringLayout.WEST, p1);
      layout.putConstraint(SpringLayout.NORTH, p2, 10,
            SpringLayout.SOUTH, p1);

      t22 = new JTextField("",5);
      contents.add(t22);
      layout.putConstraint(SpringLayout.WEST, t22, 0,
            SpringLayout.WEST, h2);
      layout.putConstraint(SpringLayout.NORTH, t22, 0,
            SpringLayout.NORTH, p2);
      t23 = new JTextField("",5);
      contents.add(t23);
      layout.putConstraint(SpringLayout.WEST, t23, 0,
            SpringLayout.WEST, h3);
      layout.putConstraint(SpringLayout.NORTH, t23, 0,
            SpringLayout.NORTH, p2);

      p3 = new JComboBox(list);
      p3.addActionListener(new ActionListener () {
         public void actionPerformed(ActionEvent e) {
            int i = p3.getSelectedIndex();
            if (i == 0) {
               t32.setText("");
               t33.setText("");
            } else {
               String s = pt.getValue(i-1)+"";
               t32.setText(s);
               t33.setText(s);
            }
         }
      });
      contents.add(p3);
      layout.putConstraint(SpringLayout.WEST, p3, 0,
            SpringLayout.WEST, p1);
      layout.putConstraint(SpringLayout.NORTH, p3, 10,
            SpringLayout.SOUTH, p2);

      t32 = new JTextField("",5);
      contents.add(t32);
      layout.putConstraint(SpringLayout.WEST, t32, 0,
            SpringLayout.WEST, h2);
      layout.putConstraint(SpringLayout.NORTH, t32, 0,
            SpringLayout.NORTH, p3);
      t33 = new JTextField("",5);
      contents.add(t33);
      layout.putConstraint(SpringLayout.WEST, t33, 0,
            SpringLayout.WEST, h3);
      layout.putConstraint(SpringLayout.NORTH, t33, 0,
            SpringLayout.NORTH, p3);

      // Buttons
      JButton okButton = new JButton();
      okButton.setText(" OK ");
      okButton.setActionCommand(CMD_OK);
      okButton.addActionListener(new ActionListener() {
         public void actionPerformed(ActionEvent event) {
            windowAction(event);
         }
      });
      contents.add(okButton);
      layout.putConstraint(SpringLayout.WEST, okButton, 20,
            SpringLayout.WEST, contents);
      layout.putConstraint(SpringLayout.NORTH, okButton, 30,
            SpringLayout.SOUTH, p3);

      // cancel button
      JButton cancelButton = new JButton();
      cancelButton.setText(" Cancel ");
      cancelButton.setActionCommand(CMD_CANCEL);
      cancelButton.addActionListener(new ActionListener() {
         public void actionPerformed(ActionEvent event) {
            windowAction(event);
         }
      });
      contents.add(cancelButton);
      layout.putConstraint(SpringLayout.EAST, cancelButton, -20,
            SpringLayout.EAST, contents);
      layout.putConstraint(SpringLayout.SOUTH, cancelButton, 0,
            SpringLayout.SOUTH, okButton);

      // space
      
      layout.putConstraint(SpringLayout.EAST, contents, 10,
            SpringLayout.EAST, ltlText);
      layout.putConstraint(SpringLayout.SOUTH, contents, 10,
            SpringLayout.SOUTH, okButton);

      // Final touches on the buttons...
      getRootPane().setDefaultButton(okButton);

   }

   public String getResult() {
      return result;
   }
   
   /**
    * The user has selected an option.
    * If actionCommand is an ActionEvent, getCommandString() is called,
    * otherwise toString() is used to get the action command.
    *
    * @param actionCommand may be null
    */
   private void windowAction(Object actionCommand) {
      boolean closeWindow = false;
      String cmd = null;
      if (actionCommand != null) {
         if (actionCommand instanceof ActionEvent) {
            cmd = ((ActionEvent)actionCommand).getActionCommand();
         } else {
            cmd = actionCommand.toString();
         }
      }
      if (cmd == null) {
         // do nothing
      } else if (cmd.equals(CMD_CANCEL)) {
         result = null;
         closeWindow = true;
      } else if (cmd.equals(CMD_OK)) {
         String error="";
         try {
            StringBuffer buf = new StringBuffer("learn_parameters([");
            if (p1.getSelectedIndex()>0) {
               buf.append((String) p1.getSelectedItem());
               if (p2.getSelectedIndex()>0) {
                  buf.append(", "+p2.getSelectedItem());
                  if (p3.getSelectedIndex()>0)
                     buf.append(", "+p3.getSelectedItem());
               }
               buf.append("], [");

               error = t12.getText();
               buf.append("("+Float.parseFloat(error));
               error = t13.getText();
               buf.append(", "+Float.parseFloat(error)+")");

               if (p2.getSelectedIndex()>0) {
                  error = t22.getText();
                  buf.append(", ("+Float.parseFloat(error));
                  error = t23.getText();
                  buf.append(", "+Float.parseFloat(error)+")");
                  if (p3.getSelectedIndex()>0) {
                     error = t32.getText();
                     buf.append(", ("+Float.parseFloat(error));
                     error = t33.getText();
                     buf.append(", "+Float.parseFloat(error)+")");
                  }
               }
               
               error = stepsText.getText();
               int steps = Integer.parseInt(error);
               buf.append("], "+steps+", "+ltlText.getText()+", ");
               error = timeText.getText();
               float time = Float.parseFloat(error);
               buf.append(time+").\n");

               result = buf.toString();
            } else {
               result = null;
            }
            closeWindow = true;
         } catch  (Exception excp) {
            JOptionPane.showMessageDialog(this,
                  "Invalid value: "+error,
                  "Invalid value",
                  JOptionPane.WARNING_MESSAGE);
         }
      } else {
         System.out.println("Command invoked: " + cmd);
      }
      if (closeWindow) {
         setVisible(false);
         dispose();
      }
   }
}
