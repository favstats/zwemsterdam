// Fixed Sloterparkbad - correct locationId and URL
const { chromium } = require('playwright');

async function main() {
  const browser = await chromium.launch({ headless: true });
  const context = await browser.newContext({
    userAgent: 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36'
  });
  const page = await context.newPage();
  
  try {
    // Navigate to CORRECT Sloterparkbad URL
    console.log('📍 Navigating to zwembad-het-sloterparkbad-amsterdam...');
    await page.goto('https://www.optisport.nl/zwembad-het-sloterparkbad-amsterdam', { 
      waitUntil: 'networkidle', 
      timeout: 60000 
    });
    await page.waitForFunction(() => !document.title.includes('Just a moment'), { timeout: 30000 });
    await page.waitForTimeout(2000);
    console.log('✅ Cloudflare passed\n');
    
    // Get token
    const token = await page.evaluate(async () => {
      const res = await fetch('/api/optisport/token', { credentials: 'same-origin' });
      return res.ok ? await res.text() : null;
    });
    console.log('🔑 Token:', token?.substring(0, 30) + '...');
    
    // Try schedule API with CORRECT locationId 2305
    console.log('\n📅 Trying schedule API with locationId 2305...');
    const scheduleResult = await page.evaluate(async ({ token }) => {
      const res = await fetch('https://www.optisport.nl/api/optisport/v1/schedule', {
        method: 'POST',
        credentials: 'same-origin',
        headers: {
          'Content-Type': 'application/json',
          'X-CSRF-Token': token
        },
        body: JSON.stringify({ page: 0, locationId: 2305, results: 50 })
      });
      
      const text = await res.text();
      let json = null;
      try { json = JSON.parse(text); } catch(e) {}
      return { status: res.status, json, text: text.substring(0, 500) };
    }, { token });
    
    console.log('   Status:', scheduleResult.status);
    
    if (scheduleResult.json?.schedule) {
      console.log('   ✅ SUCCESS! Days:', scheduleResult.json.schedule.length);
      
      let totalEvents = 0;
      scheduleResult.json.schedule.forEach(day => {
        totalEvents += day.events?.length || 0;
      });
      console.log('   Total events:', totalEvents);
      
      // Show sample
      console.log('\n📋 Sample events:');
      scheduleResult.json.schedule.slice(0, 3).forEach(day => {
        console.log(`\n   ${day.day} ${day.date}:`);
        day.events?.slice(0, 2).forEach(e => {
          console.log(`      - ${e.title}: ${e.start?.substring(11, 16)} - ${e.end?.substring(11, 16)}`);
        });
      });
    } else {
      console.log('   Response:', scheduleResult.text);
    }
    
  } catch (error) {
    console.error('❌ Error:', error.message);
  } finally {
    await browser.close();
  }
}

main();

