// Playwright script to fetch Optisport data (bypasses Cloudflare)
const { chromium } = require('playwright');
const fs = require('fs');

const API_URL = 'https://www.optisport.nl/api/optisport/v1/schedule';

// Known pools with their IDs
const POOLS = [
  { 
    name: 'Bijlmer Sportcentrum', 
    url: 'https://www.optisport.nl/zwembad-bijlmer-amsterdam-zuidoost',
    locationId: 2202
  },
  { 
    name: 'Sloterparkbad', 
    url: 'https://www.optisport.nl/zwembad-het-sloterparkbad-amsterdam',
    locationId: 2305  // NOT 2304!
  },
];

async function main() {
  console.log('🏊 Optisport Data Fetcher\n');
  
  const browser = await chromium.launch({ headless: true });
  const context = await browser.newContext({
    userAgent: 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36'
  });
  const page = await context.newPage();
  
  const allPoolData = {};
  let csrfToken = null;
  
  try {
    // First, get a valid session by visiting the first pool
    console.log('📍 Getting valid session from Bijlmer page...');
    await page.goto(POOLS[0].url, { waitUntil: 'networkidle', timeout: 60000 });
    await page.waitForFunction(() => !document.title.includes('Just a moment'), { timeout: 30000 });
    await page.waitForTimeout(2000);
    console.log('✅ Cloudflare passed');
    
    // Get CSRF token
    csrfToken = await page.evaluate(async () => {
      const res = await fetch('/session/token', { credentials: 'same-origin' });
      return res.ok ? await res.text() : null;
    });
    console.log('🔑 CSRF Token:', csrfToken ? csrfToken.substring(0, 30) + '...' : 'null');
    
    // Now fetch all pools using the same session
    for (const pool of POOLS) {
      console.log(`\n📅 Fetching ${pool.name} (ID: ${pool.locationId})...`);
      
      let allSchedule = [];
      let currentPage = 1;
      let hasMore = true;
      
      while (hasMore && currentPage <= 10) {
        const result = await page.evaluate(async ({ apiUrl, locationId, pageNum, token }) => {
          const res = await fetch(apiUrl, {
            method: 'POST',
            credentials: 'same-origin',
            headers: {
              'Content-Type': 'application/json',
              'Accept': 'application/json',
              ...(token ? { 'X-CSRF-Token': token } : {})
            },
            body: JSON.stringify({ page: pageNum - 1, locationId, results: 50 })  // API uses 0-indexed pages
          });
          
          if (!res.ok) {
            return { error: res.status, message: await res.text() };
          }
          return await res.json();
        }, { apiUrl: API_URL, locationId: pool.locationId, pageNum: currentPage, token: csrfToken });
        
        if (result.error) {
          console.log(`   ❌ Error: ${result.error} - ${result.message?.substring(0, 100)}`);
          break;
        }
        
        if (result.schedule?.length > 0) {
          allSchedule = allSchedule.concat(result.schedule);
          console.log(`   Page ${currentPage}: ${result.schedule.length} days`);
        } else {
          console.log(`   Page ${currentPage}: no data`);
        }
        
        if (result.next_page && result.next_page > currentPage) {
          currentPage = result.next_page;
        } else {
          hasMore = false;
        }
      }
      
      if (allSchedule.length > 0) {
        let events = [];
        for (const day of allSchedule) {
          for (const event of day.events || []) {
            events.push({ ...event, dateLabel: day.date, dayName: day.day });
          }
        }
        
        console.log(`   ✅ Total: ${events.length} events across ${allSchedule.length} days`);
        
        const activities = [...new Set(events.map(e => e.title))];
        console.log(`   🏊 Activities: ${activities.slice(0, 5).join(', ')}${activities.length > 5 ? '...' : ''}`);
        
        allPoolData[pool.name] = {
          locationId: pool.locationId,
          schedule: allSchedule,
          events,
          fetchedAt: new Date().toISOString()
        };
      } else {
        console.log(`   ⚠️  No schedule data available`);
      }
    }
    
    // Save
    if (Object.keys(allPoolData).length > 0) {
      if (!fs.existsSync('data')) fs.mkdirSync('data');
      fs.writeFileSync('data/optisport_data.json', JSON.stringify(allPoolData, null, 2));
      console.log('\n💾 Saved to data/optisport_data.json');
      
      console.log('\n📊 Summary:');
      for (const [name, data] of Object.entries(allPoolData)) {
        console.log(`   ${name}: ${data.events.length} events (ID: ${data.locationId})`);
      }
    }
    
  } catch (error) {
    console.error('❌ Error:', error.message);
  } finally {
    await browser.close();
  }
}

main();

