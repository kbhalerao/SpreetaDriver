/* Code for SPREETA TSPR1A170100 
   Based on datasheet SLYS009B - March 2000
   
   Kaustubh D. Bhalerao
   bhalerao@illinois.edu
   Part of the openBioinstrumentation project.
   
   License: MIT

  Revisions
  
  Nov 6 2014: Removed glitch for first measurement
              Built a calibration routine
              Modified code to use Serial.parseInt()
              Modified code to do block reads for the air raw data. 
              (Not working: EEPROM Write)
   
  Nov 5 2014: Initial draft. Sensor reads, both the ADC and the EEPROM. 
              Calibration routine and EEPROM writes not enabled. 
              Sensor needs a really dark environment to work correctly
     
*/

#include <Wire.h>

//----------------------
// Hardware allocation
//----------------------

#define CLOCK 11 
// this is on Timer 2
#define START 3
#define DUTY_CYCLE 25 // 25/256 ~ 10%
#define AININPUT A0 // Pin to measure analog output
#define LED 4
#define INDICATOR 13
#define EEPSCL A5
#define EEPSDA A4

//----------------------
// State names
//----------------------

#define STATE_IDLE 0
#define STATE_MEASURING 1
#define STATE_EEPROM_READ 2
#define STATE_CALIBRATE 3
#define STATE_WRITE_AIRREF 4
#define STATE_STREAMING 5

//----------------------
// State phases
//----------------------
#define PHASE_INIT 0
#define PHASE_RUN 1
#define PHASE_EXIT 2

//----------------------
// Constants
//----------------------
#define MEASURE_INTERVAL 1000 // ms
#define BUFFERSIZE 128
#define EEPROM_ADDRESS 0x51
#define READ_EEPROM 161
#define WRITE_EEPROM 160

//----------------------
// Serial commands
//----------------------
#define READ_DATA 1
#define READ_EEPROM 2
#define CALIBRATE 3
#define WRITE_AIRREF 4
#define READ_STREAM 5
#define READ_STREAM_OFF 6

#define SP(x) Serial.println(x)

//----------------------
// Variables
//----------------------

volatile byte state = STATE_IDLE; 
volatile byte phase = PHASE_INIT; // running in the idle state
volatile unsigned long timestamp; 
volatile unsigned int reading[BUFFERSIZE]; 
volatile byte reading_index; 

int led_intensity = 100; // max led intensity
int led_ontime = 18; // max led intensity
bool streaming = false; 

void setup() {
  
  pinMode(CLOCK, OUTPUT);
  pinMode(START, OUTPUT); 
  digitalWrite(START, LOW);
  pinMode(INDICATOR, OUTPUT); 
  
  pinMode(LED, OUTPUT); 
  digitalWrite(LED, LOW); 
  
  // Set up clock for the spreeta
  // reeta master clock rate = 5 KHz (say)
  // Period = 1/5 = 0.2 ms = 200 us. 
  // Set up fast PWM, where fPWM = f_io / N.256
  // f_io = 16 MHz, N = 8, 
  // f_PWM = 7.8125 KHz. 
  // T_PWM (period) = 1280 us
  // Duty cycle ~ 10%
  
  TCCR2A |= (1<<COM2A1); 
  // Clear OC0A on Compare Match, set OC0A at BOTTOM, (non-inverting mode).
  TCCR2A |= (1<<WGM21|1<<WGM20); 
  // Mode 3 Fast PWM, Top at 0xFF, Update of OCRx at Bottom
  TCCR2B |= (1<<CS21); // prescaler at 8
  OCR2A = DUTY_CYCLE; // reset at 10% duty cycle
    
  // set up ADC
  // ADC notes
  // Conversion takes 13.5 cycles on the ADC
  // Max resolution is when ADC clock is between 50 and 200 KHz
  // Clock rate is 16 MHz - therefore dividing factor should be
  //       128 - to give a clock rate of 125 KHz
  // Period = 1/125e3 = 0.008 ms = 8 us. 
  // Conversion time = 8 * 13.5 = 108 us. 
  // ADMUX REGISTER 
  // Use 3.3 V on the AREF pin from the Arduino
  // REFS1 and REFS0 are zero. 
  // ADLAR is zero - we will use 10 bit resolution
  // MUX is set to zero - Analog readings on A0
  // ADCSRA register
  ADCSRA |= (1<<ADEN)|(1<<ADSC); // enable ADC and fire off first conversion
  ADCSRA |= (1<<ADPS2)|(1<<ADPS1)|(1<<ADPS0); // prescale to 128
  // DIDR0 register
  DIDR0 |= (1<<ADC0D); // disable input buffer on pin
  //en_ints(); // enable ADC and timer interrupts
  
  Serial.begin(9600);
  Wire.begin(); 
  
}

void dump_data(void); 
void read_eeprom(void); 
void readblock(int address, int start, int bytes, bool in_line);

void loop() {
  // put your main code here, to run repeatedly:
  switch(state) {
    case STATE_IDLE:
      switch(phase) {
        case PHASE_INIT:
          phase = PHASE_RUN;
          timestamp = millis(); 
          break; 
        case PHASE_RUN:
          // check for serial mailbox
          if(Serial.available() >0) {
            int value = Serial.parseInt(); 
            Serial.println(value, DEC);
            switch (value) {
              case READ_DATA:
                state = STATE_MEASURING; 
                phase = PHASE_INIT; 
                TIMSK2 |= (1<<TOIE2); // Set interrupt on compare
                break;
              case READ_EEPROM:
                read_eeprom(); 
                break; 
              case CALIBRATE:
                state = STATE_CALIBRATE; 
                phase = PHASE_INIT; 
                break; 
              case WRITE_AIRREF:
                // TODO
                break; 
              case READ_STREAM:
                streaming = true; 
                break;
              case READ_STREAM_OFF: 
                streaming = false; 
                break;
            }
          }
          
          if (streaming) {
            if(millis() - timestamp >= MEASURE_INTERVAL) {
              state = STATE_MEASURING; 
              phase = PHASE_INIT; 
              TIMSK2 |= (1<<TOIE2);
            }
          }
          break; 
          
        case PHASE_EXIT:
          // never gets here!
          break; 
      }
      break;
      
    case STATE_MEASURING:
      // in this state we pulse the start pin, and start
      // a counter for the reset phase 
      // we also kick off ADC measurements
      
      switch (phase) {
        case PHASE_INIT:
          // the interrupt sends it to phase_run
          break;
        case PHASE_RUN: 
          
          if(reading_index >= led_ontime) {
            digitalWrite(LED, HIGH); 
          }
          if(reading_index >= BUFFERSIZE) {
            phase = PHASE_EXIT;
          }
          break; 
        case PHASE_EXIT: 
          digitalWrite(LED, LOW); 
          state = STATE_IDLE; 
          phase = PHASE_INIT; 
          TIMSK2 &= ~(1<<TOIE2); // disable interrupts
          ADCSRA &= ~(1<<ADIE);
          dump_data(); // write out the data to the console
          break;
      }
      break;
      
      case STATE_CALIBRATE: 
        switch(phase) {
          case PHASE_INIT:
            Serial.print("Current: ");
            Serial.print(led_intensity, DEC); 
            Serial.println(" LED intensity (0-100)?"); 
            phase = PHASE_RUN; 
            break;
          case PHASE_RUN:
            if(Serial.available() >0) {
              led_intensity = Serial.parseInt();
              Serial.println(led_intensity, DEC); 
              led_ontime = map(led_intensity, 100, 0, 18, 128); 
              Serial.println(led_ontime, DEC); 
              phase = PHASE_EXIT;
            }
            break; 
          case PHASE_EXIT:
            Serial.println("OK");
            state = STATE_IDLE; 
            phase = PHASE_INIT; 
            break; 
        }
        break; 
      
  }
}

//// ISR Vector TIMER0 COMPA
ISR(TIMER2_OVF_vect) {
  if(state == STATE_MEASURING) {
    switch(phase) {
      case PHASE_INIT:
        // toggle the START pin
        digitalWrite(START, HIGH); 
        phase = PHASE_RUN;
        reading_index = 0;
        ADCSRA |= (1<<ADIE)|(1<<ADCSRA); // enable interrupt and do first conversion
        break; 
      case PHASE_RUN:
        ADCSRA |= (1<<ADSC); // kick off conversion here. 
        digitalWrite(INDICATOR, HIGH);  
        digitalWrite(START, LOW); 
        break;
    } 
  }
}
//
ISR(ADC_vect) {
  if(state == STATE_MEASURING) {
    digitalWrite(INDICATOR, LOW); 
    reading[reading_index] = ADCL | (ADCH <<8); 
    reading_index++; 
  }
}

void dump_data(void) {
  Serial.print("T: "); 
  Serial.println(timestamp, DEC); 
  Serial.println("Data"); 
  for (int i=0; i<BUFFERSIZE; i++) {
    // dump out the read buffer
    Serial.print(i, DEC);
    Serial.print(":");
    double volts = (double) reading[i] / 1024.0 * 3.3; 
    Serial.println(volts, DEC);
  } 
  Serial.println("OK");
}

void read_eeprom(void) {
  Serial.println("Air ref");
  readblock(EEPROM_ADDRESS, 1, 256, false); 
  Serial.print("Model :");
  readblock(EEPROM_ADDRESS, 262, 12, true); 
  Serial.print("ID: "); 
  readblock(EEPROM_ADDRESS, 274, 20, true); 
  Serial.println("Other junk");
  readblock(EEPROM_ADDRESS, 294, 24, false); 
}
 
void readblock(int add, int start, int num, bool in_line) {
  
  int reqsize = 1; 
  if(start == 1) {
    // reading the air ref - we can do a block read instead
    reqsize = 16; 
  }
  
  while(num > 0) {
    Wire.beginTransmission(add); 
    Wire.write(start); 
    Wire.endTransmission(); 
  
    Wire.requestFrom(add, reqsize); 
    while(Wire.available()) {
      char a = Wire.read(); 
      if(in_line && (a>32 || a<127)) {
        Serial.print(a); 
      }
      else if(in_line && !(a>32 || a<127)) {
        Serial.print(a, DEC); 
      }
      else {
        Serial.println(a, DEC); 
      }
    }
    start = start + reqsize;
    num = num - reqsize;
    delay(1); 
  }
  if(in_line) {
    Serial.println(""); 
  }
}
  
