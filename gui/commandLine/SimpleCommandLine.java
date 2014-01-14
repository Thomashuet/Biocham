package fr.inria.contraintes.biocham.commandLine;


import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.WorkbenchArea;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.Vector;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.text.BadLocationException;
import javax.swing.tree.DefaultMutableTreeNode;



/**
 * A class representing a command-line interface to Biocham, like a custom panel that looks like the native command-line interface.
 *           
 * @author Sylvain Soliman, Dragana JOVANOVSKA.
*/
public class SimpleCommandLine extends JPanel implements ActionListener{

	
	JScrollPane scrollPane;
	JTextField input;
	JTextArea output;
	BiochamModel model;
	Vector<String> historyVector;
	int historyIndex;
	
	
	
	public SimpleCommandLine(final DefaultMutableTreeNode p,final BiochamModel m){
		
		SwingWorker sw=new SwingWorker(){

			@Override
			public Object construct() {
				buildPanel(p, m);
				return null;
			}

			@Override
			public void finished() {
				addCommandLine(p);
			}
			
			
		};
		sw.start();
		
	}

	/**
	 * It adds the commandline task to the model's tree.
	 * 
	 * @param p
	 */
	private void addCommandLine(final DefaultMutableTreeNode p) {
		WorkbenchArea.tree.addCommandLineNodeToTree(p,this);
	}

	/**
	 * 
	 * It constructs the commandline panel with its 3 text areas.
	 * 
	 * @param p
	 * @param m
	 */
	private void buildPanel(DefaultMutableTreeNode p, BiochamModel m) { //Create the console Panel
		  historyVector = new Vector<String>();
	      historyIndex = -1;
	      model=m;
	      model.setHistoryIndex(historyIndex);
	      model.setHistoryVector(historyVector);
	      
	      setLayout(new GridBagLayout());
	      //setBackground(Utils.backgroundColor);
	      GridBagConstraints c = new GridBagConstraints();	      
	      setOpaque(true);	      
	      
	      c.gridx=0;
	      c.gridy=0;
	      JLabel l=new JLabel("Command: ");
	      l.setBackground(Color.blue);
	      add(l, c);
	      
	      input = new JTextField(80);
	      
	      input.addActionListener(this);
	      // avoid TAB events consumption by FocusTraversal
	      // keep them for command completion
	      input.setFocusTraversalKeysEnabled(false);
	      input.addKeyListener(new KeyAdapter(){
	    	  public void keyPressed(KeyEvent e) {
	    	      // History
	    	      if (e.getKeyCode() == KeyEvent.VK_UP && !historyVector.isEmpty()) {
	    	         if (historyIndex < 0)
	    	            historyIndex = historyVector.size()-1;
	    	         else
	    	            if (historyIndex > 0)
	    	               historyIndex--;
	    	         input.setText(historyVector.elementAt(historyIndex));
	    	      }
	    	      if (e.getKeyCode() == KeyEvent.VK_DOWN && historyIndex >= 0) {
	    	         if (historyIndex < historyVector.size()-1) {
	    	            historyIndex++;
	    	            input.setText(historyVector.elementAt(historyIndex));
	    	         }
	    	         else {
	    	            historyIndex=-1;
	    	            input.setText(null);
	    	         }
	    	      }
	    	      // Command completion
	    	      if (e.getKeyCode() == KeyEvent.VK_TAB) {
	    	         input.setText(BiochamCommands.getContinuation(input.getText()));
	    	      }
	    	   }
	      });
	      
	      c.gridx=1;
	      c.gridy=0;
	      c.fill=GridBagConstraints.HORIZONTAL;
	      add(input, c);
	      
	      //Create a scrolled text area.
	      output = new JTextArea(5, 30);
	     /* output.setBackground(Color.black);
	      output.setForeground(Color.white);*/
	      output.setEditable(false);
	      output.setFont(new Font("Monospaced",Font.PLAIN,12));
	      output.setWrapStyleWord(true);	 
	      output.setLineWrap(true);
	      output.setCaretPosition(output.getDocument().getLength());
	      scrollPane = new JScrollPane(output);
	      scrollPane.setAutoscrolls(true);
	      //Add the text area to the panel.
	      c.gridx=0;
	      c.gridy=1;
	      c.fill=GridBagConstraints.BOTH;
	      c.gridwidth=2;
	      c.weightx=1.0;
	      c.weighty=1.0;
	      add(scrollPane, c);
	      Utils.debugMsg("******************************************doc length= "+output.getDocument().getLength());
	}
	
	
	public void actionPerformed(ActionEvent e) {
		 if (e.getSource() == input) {
	         String s = input.getText()+"\n";
	         Utils.debugMsg(s);
	         model.sendToBiocham(s);//.getBiochamInput().println(s);
	         /*if (historyVector.size() > 31){
	            historyVector.removeElementAt(0);
	         }*/
	         //historyVector.add(s);
	         output.append(s);
	       //  System.out.println("doc length= "+output.getDocument().getLength());
	       //  output.setCaretPosition(output.getDocument().getLength());
	        // scrollPane.setPreferredSize(new Dimension(200,200));
	         input.setText(null);
	         //historyIndex = -1;
	      }

		 
	      output.setCaretPosition(output.getDocument().getLength());
	      
	      
	}
	
	/** 
     * This method send a command to Biocham. It writes directely to the stream of communication with Biocham.
     *
     * @param s         The command as a String
     */
	public void sendCommandToBiocham(String s){
		model.sendToBiocham(s,"cmdLine");			
	}
	
	/** 
     * This method gets the response of Biocham for the previously entered command in the direct communication. It writes the response to the output area 
     * of the command prompt.
     *
     * @param s         The answer(output) of Biocham.
     */
	public void outputFromBiocham(String s) throws BadLocationException{		
		output.append(s);	
		output.setCaretPosition(output.getDocument().getLength());
	}
	
	public void inputToBiocham(String s) throws BadLocationException{		
		output.append(s);	
		output.setCaretPosition(output.getDocument().getLength());
	}
	
	
	public String toString(){
		return "Biocham Command-Line";
	}
	public BiochamModel getModel() {
		return model;
	}
	public void setModel(BiochamModel model) {
		this.model = model;
	}
}
