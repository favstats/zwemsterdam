import React, { useState, useEffect, useMemo } from 'react';
import { Search, MapPin, Clock, Calendar, ChevronDown, ChevronUp, Waves, List, Navigation, ExternalLink, Info, AlertTriangle, X, Database, Github, BarChart3, Coffee } from 'lucide-react';

// ============================================================================
// Types
// ============================================================================

interface SwimmingSession {
  id: number;
  bad: string;
  dag: string;
  date?: string;
  activity: string;
  extra: string;
  start: number;
  end: number;
  website?: string;
}

interface DataSource {
  name: string;
  description: string;
  url: string;
  pools: string[];
}

interface Metadata {
  lastUpdated: string;
  lastUpdatedLocal: string;
  totalSessions: number;
  pools: string[];
  dataSources: DataSource[];
}

// ============================================================================
// Constants
// ============================================================================

const DUTCH_DAYS = ['Zondag', 'Maandag', 'Dinsdag', 'Woensdag', 'Donderdag', 'Vrijdag', 'Zaterdag'];
const DAY_ABBREVIATIONS: Record<string, string> = {
  'Maandag': 'Ma',
  'Dinsdag': 'Di',
  'Woensdag': 'Wo',
  'Donderdag': 'Do',
  'Vrijdag': 'Vr',
  'Zaterdag': 'Za',
  'Zondag': 'Zo',
};

// Pool colors - distinct and vibrant
// Currently available pools from API/scrapers:
// - Municipal (Amsterdam API): Zuiderbad, Noorderparkbad, De Mirandabad, Flevoparkbad, Brediusbad
// - Het Marnix (own API)
// - Sportfondsen: Sportfondsenbad Oost, Sportplaza Mercator
// Note: Sloterparkbad & Bijlmer Sportcentrum (Optisport) require browser automation (Cloudflare)
const POOL_COLORS: Record<string, string> = {
  'Zuiderbad': '#C8102E',           // Amsterdam Red
  'Het Marnix': '#0077B6',          // Canal Blue
  'Sportplaza Mercator': '#059669', // Emerald
  'De Mirandabad': '#7C3AED',       // Purple
  'Noorderparkbad': '#0891B2',      // Cyan
  'Flevoparkbad': '#16A34A',        // Green
  'Brediusbad': '#DC2626',          // Red
  'Sportfondsenbad Oost': '#8B5CF6', // Violet
};

// Day colors for multi-day view
const DAY_COLORS: Record<string, string> = {
  'Maandag': '#C8102E',
  'Dinsdag': '#0077B6',
  'Woensdag': '#059669',
  'Donderdag': '#7C3AED',
  'Vrijdag': '#EA580C',
  'Zaterdag': '#0891B2',
  'Zondag': '#BE185D',
};

// Pool website fallbacks (in case not in data)
const POOL_WEBSITES: Record<string, string> = {
  'Zuiderbad': 'https://www.amsterdam.nl/zuiderbad/zwembadrooster-zuiderbad/',
  'Noorderparkbad': 'https://www.amsterdam.nl/noorderparkbad/zwembadrooster-noorderparkbad/',
  'De Mirandabad': 'https://www.amsterdam.nl/de-mirandabad/zwembadrooster-de-mirandabad/',
  'Flevoparkbad': 'https://www.amsterdam.nl/flevoparkbad/zwembadrooster-flevoparkbad/',
  'Brediusbad': 'https://www.amsterdam.nl/brediusbad/zwembadrooster-brediusbad/',
  'Het Marnix': 'https://hetmarnix.nl/zwemmen/',
  'Sportfondsenbad Oost': 'https://amsterdamoost.sportfondsen.nl/tijden-tarieven/',
  'Sportplaza Mercator': 'https://mercator.sportfondsen.nl/tijden-tarieven/',
};

const MIN_TIME = 6;
const MAX_TIME = 22;
const TIME_RANGE = MAX_TIME - MIN_TIME;

// Activity categories
const SWIMMING_ACTIVITIES = ['Banenzwemmen', 'Recreatiezwemmen', 'Dameszwemmen', 'Naaktzwemmen', 'Zwangerschapszwemmen', 'Duurtraining', 'Peuter-kleuterzwemmen', 'Zwemles', 'Recreatief', 'Zwemplezier'];
const AQUA_ACTIVITIES = ['Aqua', 'Float', 'Aquajoggen'];
const CLOSED_ACTIVITIES = ['Gesloten', 'gesloten'];
const NON_SWIMMING_ACTIVITIES = ['Squash', 'Fifty Fit'];

// Categorize activity
const getActivityCategory = (activity: string): 'swimming' | 'aqua' | 'closed' | 'other' => {
  if (CLOSED_ACTIVITIES.some(c => activity.toLowerCase().includes(c.toLowerCase()))) return 'closed';
  if (SWIMMING_ACTIVITIES.some(s => activity.toLowerCase().includes(s.toLowerCase()))) return 'swimming';
  if (AQUA_ACTIVITIES.some(a => activity.toLowerCase().includes(a.toLowerCase()))) return 'aqua';
  if (NON_SWIMMING_ACTIVITIES.some(n => activity.toLowerCase().includes(n.toLowerCase()))) return 'other';
  return 'swimming'; // Default to swimming
};

// Check if session has a note worth highlighting
const hasImportantNote = (session: SwimmingSession): boolean => {
  if (!session.extra) return false;
  const importantKeywords = ['minder', 'beperkt', 'gesloten', 'reserveren', 'vol', 'geen'];
  return importantKeywords.some(k => session.extra.toLowerCase().includes(k));
};

// ============================================================================
// Utility Functions
// ============================================================================

const formatTime = (decimalTime: number): string => {
  const hours = Math.floor(decimalTime);
  const minutes = Math.round((decimalTime - hours) * 60);
  return `${hours}:${minutes.toString().padStart(2, '0')}`;
};

const getCurrentDecimalTime = (): number => {
  const now = new Date();
  return now.getHours() + now.getMinutes() / 60;
};

const getTodayDutch = (): string => {
  return DUTCH_DAYS[new Date().getDay()];
};

const isSessionNow = (session: SwimmingSession): boolean => {
  const now = getCurrentDecimalTime();
  const today = getTodayDutch();
  return session.dag === today && session.start <= now && session.end > now;
};

const deduplicateSessions = (sessions: SwimmingSession[]): SwimmingSession[] => {
  const seen = new Map<string, SwimmingSession>();
  sessions.forEach(session => {
    const key = `${session.bad}|${session.dag}|${session.activity}|${session.start.toFixed(2)}|${session.end.toFixed(2)}`;
    if (!seen.has(key)) {
      seen.set(key, session);
    }
  });
  return Array.from(seen.values());
};

const getPoolColor = (poolName: string, index: number): string => {
  return POOL_COLORS[poolName] || Object.values(POOL_COLORS)[index % Object.values(POOL_COLORS).length];
};

const getPoolWebsite = (session: SwimmingSession): string => {
  return session.website || POOL_WEBSITES[session.bad] || '#';
};

const formatLastUpdated = (isoString: string): string => {
  try {
    const date = new Date(isoString);
    return date.toLocaleDateString('nl-NL', {
      day: 'numeric',
      month: 'long',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  } catch {
    return isoString;
  }
};

// ============================================================================
// Wave XXX Logo Component
// ============================================================================

const WaveXXXLogo: React.FC<{ size?: 'sm' | 'md' | 'lg' }> = ({ size = 'md' }) => {
  const dimensions = { sm: 28, md: 40, lg: 56 };
  const dim = dimensions[size];

  return (
    <svg 
      width={dim} 
      height={dim} 
      viewBox="0 0 64 64" 
      className="flex-shrink-0"
    >
      {/* Three wave-X marks - Amsterdam XXX style - no background */}
      {/* Left X */}
      <g className="animate-wave">
        <path d="M12 20 L20 32 M20 20 L12 32" stroke="#C8102E" strokeWidth="4" strokeLinecap="round" fill="none"/>
        <path d="M10 26 Q16 22 22 26" stroke="#06B6D4" strokeWidth="2" strokeLinecap="round" fill="none" opacity="0.8"/>
      </g>
      
      {/* Center X */}
      <g className="animate-wave-delay-1">
        <path d="M26 20 L38 32 M38 20 L26 32" stroke="#C8102E" strokeWidth="4" strokeLinecap="round" fill="none"/>
        <path d="M24 26 Q32 22 40 26" stroke="#06B6D4" strokeWidth="2" strokeLinecap="round" fill="none" opacity="0.8"/>
      </g>
      
      {/* Right X */}
      <g className="animate-wave-delay-2">
        <path d="M44 20 L52 32 M52 20 L44 32" stroke="#C8102E" strokeWidth="4" strokeLinecap="round" fill="none"/>
        <path d="M42 26 Q48 22 54 26" stroke="#06B6D4" strokeWidth="2" strokeLinecap="round" fill="none" opacity="0.8"/>
      </g>
      
      {/* Bottom waves */}
      <path d="M6 44 Q18 38 32 44 Q46 50 58 44" stroke="#0077B6" strokeWidth="3" strokeLinecap="round" fill="none" opacity="0.7"/>
      <path d="M6 52 Q18 46 32 52 Q46 58 58 52" stroke="#0077B6" strokeWidth="2.5" strokeLinecap="round" fill="none" opacity="0.5"/>
    </svg>
  );
};

// ============================================================================
// Loading Skeleton Component
// ============================================================================

const LoadingSkeleton: React.FC = () => (
  <div className="bg-base-100 rounded-xl p-6 shadow-lg">
    <div className="space-y-4">
      {/* Header skeleton */}
      <div className="flex gap-4 mb-6">
        <div className="skeleton-loading h-10 w-32 rounded-lg"></div>
        <div className="flex-1 skeleton-loading h-10 rounded-lg"></div>
      </div>
      
      {/* Pool rows skeleton */}
      {[1, 2, 3, 4].map(i => (
        <div key={i} className="flex gap-4 items-center">
          <div className="skeleton-loading h-12 w-32 rounded-lg"></div>
          <div className="flex-1 skeleton-loading h-12 rounded-lg"></div>
        </div>
      ))}
    </div>
  </div>
);

// ============================================================================
// Week Selector Component - Shows current week prominently with date range
// ============================================================================

interface WeekSelectorProps {
  selectedWeek: 'this' | 'next';
  onSelectWeek: (week: 'this' | 'next') => void;
  availableDates: string[];
}

// Get week info (start/end dates, week number)
const getWeekInfo = (weekOffset: number = 0): { start: Date; end: Date; weekNumber: number } => {
  const now = new Date();
  const dayOfWeek = now.getDay();
  const monday = new Date(now);
  monday.setDate(now.getDate() - (dayOfWeek === 0 ? 6 : dayOfWeek - 1) + (weekOffset * 7));
  monday.setHours(0, 0, 0, 0);
  
  const sunday = new Date(monday);
  sunday.setDate(monday.getDate() + 6);
  
  // Calculate ISO week number
  const tempDate = new Date(monday);
  tempDate.setHours(0, 0, 0, 0);
  tempDate.setDate(tempDate.getDate() + 3 - ((tempDate.getDay() + 6) % 7));
  const week1 = new Date(tempDate.getFullYear(), 0, 4);
  const weekNumber = 1 + Math.round(((tempDate.getTime() - week1.getTime()) / 86400000 - 3 + ((week1.getDay() + 6) % 7)) / 7);
  
  return { start: monday, end: sunday, weekNumber };
};

// Get date for a day in a specific week
const getDateForDay = (dayName: string, weekOffset: number = 0): Date => {
  const dayIndex: Record<string, number> = {
    'Maandag': 1, 'Dinsdag': 2, 'Woensdag': 3, 'Donderdag': 4,
    'Vrijdag': 5, 'Zaterdag': 6, 'Zondag': 0
  };
  
  const { start } = getWeekInfo(weekOffset);
  const targetDayOffset = dayIndex[dayName] === 0 ? 6 : dayIndex[dayName] - 1; // Mon=0, Sun=6
  const result = new Date(start);
  result.setDate(start.getDate() + targetDayOffset);
  return result;
};

// Format date as "18 dec"
const formatDateShort = (date: Date): string => {
  const months = ['jan', 'feb', 'mrt', 'apr', 'mei', 'jun', 'jul', 'aug', 'sep', 'okt', 'nov', 'dec'];
  return `${date.getDate()} ${months[date.getMonth()]}`;
};

// Format date as ISO string (YYYY-MM-DD) in LOCAL timezone
const formatDateISO = (date: Date): string => {
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, '0');
  const day = String(date.getDate()).padStart(2, '0');
  return `${year}-${month}-${day}`;
};

const WeekSelector: React.FC<WeekSelectorProps> = ({ selectedWeek, onSelectWeek, availableDates }) => {
  const thisWeek = getWeekInfo(0);
  const nextWeek = getWeekInfo(1);
  
  // Check if weeks have data
  const hasThisWeekData = availableDates.some(d => {
    const date = new Date(d);
    return date >= thisWeek.start && date <= thisWeek.end;
  });
  const hasNextWeekData = availableDates.some(d => {
    const date = new Date(d);
    return date >= nextWeek.start && date <= nextWeek.end;
  });
  
  const currentWeekInfo = selectedWeek === 'this' ? thisWeek : nextWeek;
  
  return (
    <div className="flex flex-col sm:flex-row items-start sm:items-center gap-3 bg-gradient-to-r from-primary/10 to-secondary/10 rounded-xl p-4 border border-primary/20">
      {/* Week display */}
      <div className="flex items-center gap-3">
        <div className="w-10 h-10 rounded-lg bg-primary text-white flex items-center justify-center font-bold text-lg shadow-md">
          {currentWeekInfo.weekNumber}
        </div>
      <div>
          <div className="text-xs text-base-content/60 uppercase tracking-wide font-medium">
            {selectedWeek === 'this' ? 'Deze week' : 'Volgende week'}
          </div>
          <div className="font-bold text-base-content">
            {formatDateShort(currentWeekInfo.start)} – {formatDateShort(currentWeekInfo.end)}
          </div>
        </div>
      </div>
      
      {/* Week toggle */}
      <div className="flex bg-base-200 rounded-lg p-1 gap-1 sm:ml-auto">
        <button
          onClick={() => onSelectWeek('this')}
          disabled={!hasThisWeekData}
          className={`px-3 py-1.5 text-sm font-semibold rounded-md transition-all ${
            selectedWeek === 'this'
              ? 'bg-primary text-white shadow-md'
              : hasThisWeekData
                ? 'text-base-content hover:bg-base-300'
                : 'text-base-content/30 cursor-not-allowed'
          }`}
        >
          Deze week
        </button>
        <button
          onClick={() => onSelectWeek('next')}
          disabled={!hasNextWeekData}
          className={`px-3 py-1.5 text-sm font-semibold rounded-md transition-all ${
            selectedWeek === 'next'
              ? 'bg-primary text-white shadow-md'
              : hasNextWeekData
                ? 'text-base-content hover:bg-base-300'
                : 'text-base-content/30 cursor-not-allowed'
          }`}
        >
          Volgende week
        </button>
      </div>
    </div>
  );
};

// ============================================================================
// Day Pills Selector Component
// ============================================================================

interface DayPillsProps {
  selectedDay: string;
  onSelectDay: (day: string) => void;
  selectedWeek: 'this' | 'next';
  availableDates: string[];
}

const DayPills: React.FC<DayPillsProps> = ({ selectedDay, onSelectDay, selectedWeek, availableDates }) => {
  const orderedDays = ['Maandag', 'Dinsdag', 'Woensdag', 'Donderdag', 'Vrijdag', 'Zaterdag', 'Zondag'];
  const weekOffset = selectedWeek === 'this' ? 0 : 1;
  
  // Check if a specific date has data
  const hasDataForDate = (date: Date): boolean => {
    const dateStr = formatDateISO(date);
    return availableDates.includes(dateStr);
  };
  
  // Get today's date string
  const todayDate = new Date();
  const todayStr = formatDateISO(todayDate);
  const isTodayInSelectedWeek = selectedWeek === 'this';
  
  return (
    <div className="flex flex-wrap gap-2">
      {/* Today button - only show if "this week" is selected */}
      {isTodayInSelectedWeek && (
        <button
          onClick={() => onSelectDay('Today')}
          className={`day-pill px-4 py-2 rounded-full text-sm font-bold transition-all ${
            selectedDay === 'Today'
              ? 'bg-primary text-white active shadow-md'
              : 'bg-base-200 text-base-content hover:bg-base-300'
          }`}
        >
          <span className="flex items-center gap-1.5">
            <span className="w-2 h-2 bg-accent rounded-full animate-pulse"></span>
            Vandaag
          </span>
        </button>
      )}
      
      {/* All days button */}
      <button
        onClick={() => onSelectDay('All Days')}
        className={`day-pill px-4 py-2 rounded-full text-sm font-bold transition-all ${
          selectedDay === 'All Days'
            ? 'bg-secondary text-white active shadow-md'
            : 'bg-base-200 text-base-content hover:bg-base-300'
        }`}
      >
        Hele week
      </button>
      
      {/* Individual day pills with dates */}
      {orderedDays.map(day => {
        const dayDate = getDateForDay(day, weekOffset);
        const dateStr = formatDateISO(dayDate);
        const isToday = dateStr === todayStr;
        const hasData = hasDataForDate(dayDate);
        const dateDisplay = dayDate.getDate();
        
        return (
          <button
            key={`${day}-${weekOffset}`}
            onClick={() => onSelectDay(dateStr)}
            disabled={!hasData}
            className={`day-pill px-3 py-2 rounded-full text-sm font-bold transition-all relative flex items-center gap-1.5 ${
              selectedDay === dateStr
                ? 'bg-primary text-white active shadow-md'
                : hasData
                  ? 'bg-base-200 text-base-content hover:bg-base-300'
                  : 'bg-base-200 text-base-content/30 cursor-not-allowed'
            }`}
          >
            <span>{DAY_ABBREVIATIONS[day]}</span>
            <span className={`text-xs ${selectedDay === dateStr ? 'text-white/80' : 'text-base-content/50'}`}>
              {dateDisplay}
            </span>
            {isToday && (
              <span className="absolute -top-1 -right-1 w-2 h-2 bg-accent rounded-full animate-pulse"></span>
            )}
          </button>
        );
      })}
    </div>
  );
};

// ============================================================================
// Activity Chips Component - Dynamically shows ALL activities from dataset
// ============================================================================

interface ActivityChipsProps {
  selectedActivity: string;
  onSelectActivity: (activity: string) => void;
  availableActivities: string[];
}

// Smart activity categorization
const categorizeActivity = (activity: string): 'lap' | 'recreational' | 'lesson' | 'aqua' | 'special' | 'other' | 'closed' => {
  const lower = activity.toLowerCase();
  
  if (lower.includes('gesloten')) return 'closed';
  
  // Lap swimming (banenzwemmen, duurtraining)
  if (lower.includes('banenzwem') || lower.includes('duurtraining')) return 'lap';
  
  // Recreational swimming (recreatiezwemmen, vrijzwemmen, etc.)
  if (lower.includes('recreat') || lower.includes('vrijzwem') || lower.includes('zwemplezier')) return 'recreational';
  
  // Swimming lessons
  if (lower.includes('zwemles') || lower.includes('zwem-abc') || lower.includes('oefenuur') || lower.includes('zwemtechniek')) return 'lesson';
  
  // Aqua/Float activities
  if (lower.includes('aqua') || lower.includes('float')) return 'aqua';
  
  // Special swimming (ladies, naturist, pregnancy, baby, etc.)
  if (lower.includes('dames') || lower.includes('naaktz') || lower.includes('naturist') || 
      lower.includes('zwangerschap') || lower.includes('baby') || lower.includes('peuter') || 
      lower.includes('kleuter') || lower.includes('functiebeperking') || lower.includes('ouder & kind') ||
      lower.includes('fifty fit') || lower.includes('mbvo')) return 'special';
  
  // Non-swimming (squash, sauna)
  if (lower.includes('squash') || lower.includes('sauna')) return 'other';
  
  return 'recreational'; // Default to recreational for other swimming activities
};

// Get emoji for category
const getCategoryEmoji = (category: string): string => {
  switch (category) {
    case 'lap': return '🏊‍♂️';
    case 'recreational': return '🌊';
    case 'lesson': return '📚';
    case 'aqua': return '💧';
    case 'special': return '⭐';
    case 'other': return '🎾';
    default: return '🏊';
  }
};

// Get display name for category
const getCategoryName = (category: string): string => {
  switch (category) {
    case 'lap': return 'Banenzwemmen';
    case 'recreational': return 'Recreatief';
    case 'lesson': return 'Zwemlessen';
    case 'aqua': return 'Aqua & Float';
    case 'special': return 'Speciaal';
    case 'other': return 'Overig';
    default: return category;
  }
};

const ActivityChips: React.FC<ActivityChipsProps> = ({ selectedActivity, onSelectActivity, availableActivities }) => {
  const [expandedCategory, setExpandedCategory] = useState<string | null>(null);
  
  // Group activities by category
  const groupedActivities = useMemo(() => {
    const groups: Record<string, string[]> = {
      lap: [],
      recreational: [],
      lesson: [],
      aqua: [],
      special: [],
      other: [],
    };
    
    availableActivities.forEach(activity => {
      const category = categorizeActivity(activity);
      if (category !== 'closed' && groups[category]) {
        // Avoid duplicates by checking if similar activity already exists
        const isDupe = groups[category].some(existing => 
          existing.toLowerCase() === activity.toLowerCase()
        );
        if (!isDupe) {
          groups[category].push(activity);
        }
      }
    });
    
    // Sort each group alphabetically
    Object.keys(groups).forEach(key => {
      groups[key].sort((a, b) => a.localeCompare(b, 'nl'));
    });
    
    return groups;
  }, [availableActivities]);
  
  // Priority categories to show as main chips
  const mainCategories = ['lap', 'recreational', 'aqua', 'special'];
  
  // Check if an activity matches the selected filter
  const isActivitySelected = (category: string) => {
    if (selectedActivity === 'All Activities') return false;
    
    // Check if selectedActivity belongs to this category
    return groupedActivities[category]?.some(a => 
      a.toLowerCase().includes(selectedActivity.toLowerCase()) ||
      selectedActivity.toLowerCase().includes(a.toLowerCase().split(' ')[0])
    );
  };
  
  const buttonClass = (isActive: boolean) => 
    `px-3 py-1.5 rounded-full text-xs font-semibold transition-all ${
      isActive
        ? 'bg-accent text-white shadow-md'
        : 'bg-base-200 text-base-content hover:bg-base-300'
    }`;
  
  const categoryButtonClass = (category: string) => {
    const isActive = isActivitySelected(category) || 
      (selectedActivity !== 'All Activities' && categorizeActivity(selectedActivity) === category);
    return `px-3 py-1.5 rounded-full text-xs font-semibold transition-all ${
      isActive
        ? 'bg-accent text-white shadow-md'
        : expandedCategory === category
          ? 'bg-primary/20 text-primary ring-1 ring-primary'
          : 'bg-base-200 text-base-content hover:bg-base-300'
    }`;
  };
  
  const handleCategoryClick = (category: string) => {
    if (expandedCategory === category) {
      setExpandedCategory(null);
    } else {
      setExpandedCategory(category);
      // If clicking a category, select the first activity in that category as default
      const activities = groupedActivities[category];
      if (activities && activities.length > 0) {
        // Find best default: exact "Banenzwemmen" for lap, exact "Recreatiezwemmen" for recreational, etc.
        const defaultActivity = activities.find(a => {
          const lower = a.toLowerCase().trim();
          if (category === 'lap') return lower === 'banenzwemmen' || lower === 'duurtraining';
          if (category === 'recreational') return lower === 'recreatiezwemmen' || lower === 'vrijzwemmen';
          return true;
        }) || activities[0];
        onSelectActivity(defaultActivity);
      }
    }
  };
  
  return (
    <div className="space-y-2">
      {/* Main row: All + Category chips */}
      <div className="flex flex-wrap gap-2 items-center">
        <button
          onClick={() => {
            onSelectActivity('All Activities');
            setExpandedCategory(null);
          }}
          className={buttonClass(selectedActivity === 'All Activities')}
        >
          🏊 Alles
        </button>
        
        {/* Category chips */}
        {mainCategories.map(category => {
          const activities = groupedActivities[category];
          if (!activities || activities.length === 0) return null;
          
          return (
            <button
              key={category}
              onClick={() => handleCategoryClick(category)}
              className={categoryButtonClass(category)}
            >
              {getCategoryEmoji(category)} {getCategoryName(category)}
              <span className="ml-1 opacity-60 text-[10px]">({activities.length})</span>
            </button>
          );
        })}
        
        {/* Lessons category */}
        {groupedActivities.lesson.length > 0 && (
          <button
            onClick={() => handleCategoryClick('lesson')}
            className={categoryButtonClass('lesson')}
          >
            {getCategoryEmoji('lesson')} Lessen
            <span className="ml-1 opacity-60 text-[10px]">({groupedActivities.lesson.length})</span>
          </button>
        )}
        
        {/* Other (non-swimming) category - smaller */}
        {groupedActivities.other.length > 0 && (
          <button
            onClick={() => handleCategoryClick('other')}
            className={`${categoryButtonClass('other')} opacity-70`}
          >
            {getCategoryEmoji('other')} Overig
          </button>
        )}
      </div>
      
      {/* Expanded category activities */}
      {expandedCategory && groupedActivities[expandedCategory]?.length > 0 && (
        <div className="flex flex-wrap gap-1.5 pt-2 pl-4 border-l-2 border-accent/30 animate-in slide-in-from-top-2 duration-200">
          <span className="text-[10px] text-base-content/50 uppercase tracking-wide font-bold mr-2 self-center">
            {getCategoryName(expandedCategory)}:
          </span>
          {groupedActivities[expandedCategory].map(activity => {
            const isSelected = selectedActivity.toLowerCase() === activity.toLowerCase() ||
              activity.toLowerCase().includes(selectedActivity.toLowerCase());
            
            return (
              <button
                key={activity}
                onClick={() => onSelectActivity(activity)}
                className={`px-2 py-1 rounded-lg text-[11px] font-medium transition-all ${
                  isSelected
                    ? 'bg-accent text-white'
                    : 'bg-base-200/80 text-base-content/80 hover:bg-base-300'
                }`}
                title={activity}
              >
                {activity.length > 30 ? activity.substring(0, 27) + '...' : activity}
              </button>
            );
          })}
        </div>
      )}
    </div>
  );
};

// ============================================================================
// Tooltip Component (hover preview)
// ============================================================================

interface TooltipProps {
  session: SwimmingSession;
  x: number;
  y: number;
  poolColor: string;
}

const Tooltip: React.FC<TooltipProps> = ({ session, x, y, poolColor }) => {
  const isNow = isSessionNow(session);
  const category = getActivityCategory(session.activity);
  const isClosed = category === 'closed';
  const isOther = category === 'other';
  const hasNote = hasImportantNote(session);
  
  return (
    <div
      className="fixed z-50 bg-base-100 rounded-xl shadow-2xl p-4 max-w-xs pointer-events-none border-l-4 fade-in"
      style={{
        left: Math.min(x + 15, window.innerWidth - 280),
        top: Math.max(y - 10, 10),
        borderLeftColor: poolColor,
      }}
    >
      <div className="flex items-start justify-between gap-2 mb-2">
        <div className="font-bold text-lg flex items-center gap-2">
          {session.bad}
          {isClosed && <span className="badge badge-ghost badge-sm">Gesloten</span>}
          {isOther && <span className="badge badge-ghost badge-sm">Overig</span>}
        </div>
        {isNow && (
          <span className="badge badge-success badge-sm nu-open-badge">Nu open</span>
        )}
      </div>
      
      <div className="space-y-2 text-sm">
        <div className="flex items-center gap-2 text-base-content/70">
          <Calendar size={14} />
          <span className="font-medium">{session.dag}</span>
        </div>
        
        {!isClosed && (
          <div className="flex items-center gap-2">
            <Clock size={14} className="text-primary" />
            <span className="font-bold text-primary">{formatTime(session.start)} - {formatTime(session.end)}</span>
          </div>
        )}
        
        <div className="pt-2 border-t border-base-200">
          <p className="font-bold text-base">{session.activity}</p>
          {session.extra && (
            <div className={`mt-2 text-xs p-2 rounded ${hasNote ? 'bg-warning/20 text-warning-content' : 'bg-base-200'}`}>
              {hasNote && <AlertTriangle size={12} className="inline mr-1 text-warning" />}
              <span className="italic">{session.extra}</span>
            </div>
          )}
        </div>
        
        <div className="pt-2 text-xs text-accent font-semibold">
          Klik voor meer info & website →
        </div>
      </div>
    </div>
  );
};

// ============================================================================
// Session Detail Modal Component
// ============================================================================

interface SessionModalProps {
  session: SwimmingSession;
  poolColor: string;
  onClose: () => void;
}

const SessionModal: React.FC<SessionModalProps> = ({ session, poolColor, onClose }) => {
  const isNow = isSessionNow(session);
  const website = getPoolWebsite(session);
  
  // Close on escape key
  useEffect(() => {
    const handleEscape = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onClose();
    };
    window.addEventListener('keydown', handleEscape);
    return () => window.removeEventListener('keydown', handleEscape);
  }, [onClose]);
  
  return (
    <div 
      className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/50 backdrop-blur-sm fade-in"
      onClick={onClose}
    >
      <div 
        className="bg-base-100 rounded-2xl shadow-2xl max-w-md w-full overflow-hidden border-l-4 transform transition-all"
        style={{ borderLeftColor: poolColor }}
        onClick={(e) => e.stopPropagation()}
      >
        {/* Header */}
        <div className="p-6 pb-4 border-b border-base-200">
          <div className="flex items-start justify-between gap-4">
            <div>
              <h2 className="text-2xl font-black text-base-content">{session.bad}</h2>
              <div className="flex items-center gap-2 mt-1">
                {isNow && (
                  <span className="badge badge-success nu-open-badge">Nu open</span>
                )}
                <span className="badge badge-ghost">{session.dag}</span>
              </div>
            </div>
            <button 
              onClick={onClose}
              className="btn btn-ghost btn-sm btn-circle"
            >
              <X size={20} />
            </button>
          </div>
        </div>
        
        {/* Content */}
        <div className="p-6 space-y-4">
          {/* Time */}
          <div className="flex items-center gap-3 p-4 bg-primary/10 rounded-xl">
            <Clock size={24} className="text-primary flex-shrink-0" />
            <div>
              <p className="text-sm text-base-content/60">Tijdstip</p>
              <p className="text-xl font-bold text-primary">
                {formatTime(session.start)} - {formatTime(session.end)}
              </p>
            </div>
          </div>
          
          {/* Activity */}
          <div className="p-4 bg-base-200/50 rounded-xl">
            <p className="text-sm text-base-content/60 mb-1">Activiteit</p>
            <p className="text-lg font-bold">{session.activity}</p>
            {session.extra && (
              <p className="text-sm text-base-content/60 mt-2 italic">{session.extra}</p>
            )}
          </div>
          
          {/* Warning / Disclaimer */}
          <div className="flex items-start gap-3 p-4 bg-warning/10 rounded-xl text-sm">
            <AlertTriangle size={20} className="text-warning flex-shrink-0 mt-0.5" />
            <p className="text-base-content/70">
              Controleer altijd de officiële website voor de meest actuele tijden. Zwemsterdam is geen officiële bron.
            </p>
          </div>
          
          {/* Website Link */}
          <a
            href={website}
            target="_blank"
            rel="noopener noreferrer"
            className="btn btn-primary w-full gap-2"
          >
            <ExternalLink size={18} />
            Bekijk op officiële website
        </a>
      </div>
      </div>
    </div>
  );
};

// ============================================================================
// About / Data Sources Modal
// ============================================================================

interface AboutModalProps {
  metadata: Metadata | null;
  onClose: () => void;
}

const AboutModal: React.FC<AboutModalProps> = ({ metadata, onClose }) => {
  useEffect(() => {
    const handleEscape = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onClose();
    };
    window.addEventListener('keydown', handleEscape);
    return () => window.removeEventListener('keydown', handleEscape);
  }, [onClose]);
  
  return (
    <div 
      className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/50 backdrop-blur-sm fade-in"
      onClick={onClose}
    >
      <div 
        className="bg-base-100 rounded-2xl shadow-2xl max-w-lg w-full max-h-[90vh] overflow-y-auto"
        onClick={(e) => e.stopPropagation()}
      >
        {/* Header */}
        <div className="sticky top-0 bg-base-100 p-6 pb-4 border-b border-base-200 z-10">
          <div className="flex items-start justify-between gap-4">
            <div className="flex items-center gap-3">
              <div className="p-2 bg-secondary/10 rounded-xl">
                <Info size={24} className="text-secondary" />
              </div>
              <div>
                <h2 className="text-xl font-black text-base-content">Over Zwemsterdam</h2>
                <p className="text-sm text-base-content/60">Data & Bronnen</p>
              </div>
            </div>
            <button onClick={onClose} className="btn btn-ghost btn-sm btn-circle">
              <X size={20} />
        </button>
          </div>
        </div>
        
        {/* Content */}
        <div className="p-6 space-y-6">
          {/* Last Update */}
          {metadata && (
            <div className="p-4 bg-accent/10 rounded-xl">
              <div className="flex items-center gap-2 mb-2">
                <Database size={18} className="text-accent" />
                <span className="font-bold text-accent">Laatste update</span>
              </div>
              <p className="text-lg font-semibold">{formatLastUpdated(metadata.lastUpdated)}</p>
              <p className="text-sm text-base-content/60 mt-1">
                {metadata.totalSessions} zwemtijden van {metadata.pools.length} zwembaden
        </p>
      </div>
          )}
          
          {/* Data Sources */}
          <div>
            <h3 className="font-bold text-lg mb-3 flex items-center gap-2">
              <ExternalLink size={18} className="text-primary" />
              Databronnen
            </h3>
            <div className="space-y-3">
              {metadata?.dataSources.map((source, idx) => (
                <div key={idx} className="p-4 border border-base-300 rounded-xl hover:bg-base-50 transition-colors">
                  <div className="flex items-start justify-between gap-2">
                    <div>
                      <h4 className="font-bold">{source.name}</h4>
                      <p className="text-sm text-base-content/60">{source.description}</p>
                      <div className="flex flex-wrap gap-1 mt-2">
                        {(Array.isArray(source.pools) ? source.pools : [source.pools]).map(pool => (
                          <span key={pool} className="badge badge-ghost badge-sm">{pool}</span>
                        ))}
                      </div>
                    </div>
                    <a
                      href={source.url}
                      target="_blank"
                      rel="noopener noreferrer"
                      className="btn btn-ghost btn-sm btn-circle flex-shrink-0"
                    >
                      <ExternalLink size={16} />
                    </a>
                  </div>
                </div>
              ))}
            </div>
          </div>
          
          {/* Disclaimer */}
          <div className="p-4 bg-warning/10 rounded-xl">
            <div className="flex items-start gap-3">
              <AlertTriangle size={24} className="text-warning flex-shrink-0" />
              <div>
                <h4 className="font-bold text-warning mb-2">Belangrijke disclaimer</h4>
                <p className="text-sm text-base-content/70 leading-relaxed">
                  Zwemsterdam is een onofficieel hulpmiddel dat openbare data van diverse zwembaden verzamelt. 
                  <strong className="text-base-content"> Dit is geen officiële bron.</strong> Zwemtijden kunnen wijzigen zonder waarschuwing. 
                  Controleer altijd de officiële website van het zwembad voor de meest actuele informatie voordat je gaat zwemmen.
                </p>
              </div>
            </div>
          </div>
          
          {/* Quick Links to All Sources */}
          <div>
            <h4 className="font-bold mb-3">Snelle links naar officiële websites</h4>
            <div className="grid grid-cols-2 gap-2">
              {Object.entries(POOL_WEBSITES).map(([pool, url]) => (
                <a
                  key={pool}
                  href={url}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-sm text-secondary hover:underline flex items-center gap-1 p-2 hover:bg-base-200 rounded-lg transition-colors"
                >
                  <ExternalLink size={12} />
                  {pool}
                </a>
              ))}
            </div>
          </div>
          
          {/* Credits */}
          <div className="pt-4 border-t border-base-300">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-base-content/60">Gemaakt door</p>
                <p className="font-bold">Fabio Votta</p>
              </div>
              <a
                href="https://github.com/favstats"
                target="_blank"
                rel="noopener noreferrer"
                className="btn btn-ghost btn-sm gap-2"
              >
                <Github size={18} />
                @favstats
              </a>
              <a
                href="https://www.buymeacoffee.com/favstats"
                target="_blank"
                rel="noopener noreferrer"
                className="btn btn-warning btn-sm gap-2"
              >
                <Coffee size={18} />
                Buy me a coffee
              </a>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

// ============================================================================
// Global Time Indicator (shows once at top of Gantt)
// ============================================================================

interface GlobalNowIndicatorProps {
  selectedDay: string;
}

const useCurrentTime = () => {
  const [currentTime, setCurrentTime] = useState(getCurrentDecimalTime());
  
  useEffect(() => {
    const interval = setInterval(() => {
      setCurrentTime(getCurrentDecimalTime());
    }, 60000);
    return () => clearInterval(interval);
  }, []);
  
  return currentTime;
};

const GlobalNowIndicator: React.FC<GlobalNowIndicatorProps> = ({ selectedDay }) => {
  const currentTime = useCurrentTime();
  const today = getTodayDutch();
  
  const showIndicator = selectedDay === 'Today' || selectedDay === today;
  if (!showIndicator || currentTime < MIN_TIME || currentTime > MAX_TIME) return null;
  
  const position = ((currentTime - MIN_TIME) / TIME_RANGE) * 100;
  
  return (
    <div 
      className="absolute top-0 bottom-0 w-0.5 bg-error now-indicator z-30 pointer-events-none"
      style={{ left: `calc(144px + (100% - 144px) * ${position / 100})` }}
    >
      <div className="absolute top-2 left-1/2 -translate-x-1/2 bg-error text-white text-xs font-bold px-2 py-1 rounded-full whitespace-nowrap shadow-lg">
        Nu {formatTime(currentTime)}
      </div>
    </div>
  );
};

// ============================================================================
// Gantt Chart View Component
// ============================================================================

interface GanttViewProps {
  data: SwimmingSession[];
  selectedDay: string;
  onSessionClick: (session: SwimmingSession, color: string) => void;
}

const GanttView: React.FC<GanttViewProps> = ({ data, selectedDay, onSessionClick }) => {
  // Filter out "Gesloten" sessions from Gantt view - they're not useful here
  const deduplicatedData = useMemo(() => 
    deduplicateSessions(data).filter(session => getActivityCategory(session.activity) !== 'closed'), 
    [data]
  );
  const pools = useMemo(() => Array.from(new Set(deduplicatedData.map(d => d.bad))).sort(), [deduplicatedData]);
  
  const [tooltip, setTooltip] = useState<{ session: SwimmingSession; x: number; y: number; color: string } | null>(null);
  
  const uniqueDays = useMemo(() => new Set(deduplicatedData.map(d => d.dag)), [deduplicatedData]);
  const isSingleDay = uniqueDays.size === 1;
  
  // Assign rows to avoid overlaps
  const assignRows = (sessions: SwimmingSession[]): Map<number, number> => {
    const rowMap = new Map<number, number>();
    const sortedSessions = [...sessions].sort((a, b) => a.start - b.start || a.end - b.end);
    const rowEndTimes: number[] = [];
    
    sortedSessions.forEach(session => {
      let assignedRow = rowEndTimes.findIndex(endTime => endTime <= session.start);
      if (assignedRow === -1) {
        assignedRow = rowEndTimes.length;
        rowEndTimes.push(session.end);
      } else {
        rowEndTimes[assignedRow] = session.end;
      }
      rowMap.set(session.id, assignedRow);
    });
    
    return rowMap;
  };
  
  const getBarPosition = (start: number) => ((start - MIN_TIME) / TIME_RANGE) * 100;
  const getBarWidth = (start: number, end: number) => ((end - start) / TIME_RANGE) * 100;
  
  // Generate hour markers (every 2 hours for cleaner display)
  const hourMarkers = Array.from({ length: Math.floor(TIME_RANGE / 2) + 1 }, (_, i) => MIN_TIME + i * 2);
  
  if (pools.length === 0) {
    return null;
  }
  
  return (
    <>
      <div className="bg-base-100 rounded-xl shadow-lg overflow-hidden border border-base-300 relative">
        {/* Global Now Indicator - shows once for whole chart */}
        <GlobalNowIndicator selectedDay={selectedDay} />
        
        {/* Sticky header with hours */}
        <div className="sticky top-0 z-20 bg-base-100 border-b-2 border-primary/20">
          <div className="flex">
            {/* Pool column header */}
            <div className="w-36 md:w-44 flex-shrink-0 p-3 bg-base-200 font-bold text-sm text-base-content/70 flex items-center">
              <Waves size={16} className="mr-2 text-secondary" />
              Zwembad
            </div>
            
            {/* Time axis - with overflow visible and padding to prevent cutoff */}
            <div className="flex-1 relative h-14 bg-gradient-to-b from-base-200 to-base-100 overflow-visible px-6">
              {hourMarkers.map((hour, index) => {
                const isFirst = index === 0;
                const isLast = index === hourMarkers.length - 1;
                return (
                  <div
                    key={hour}
                    className="absolute top-0 bottom-0 flex flex-col items-center"
                    style={{ 
                      left: `${((hour - MIN_TIME) / TIME_RANGE) * 100}%`, 
                      transform: isFirst ? 'translateX(0)' : isLast ? 'translateX(-100%)' : 'translateX(-50%)' 
                    }}
                  >
                    <div className="w-px h-3 bg-primary/40"></div>
                    <div className="mt-1 text-sm font-bold text-primary bg-base-100 px-2 py-0.5 rounded shadow-sm whitespace-nowrap">
                      {hour}:00
                    </div>
                  </div>
                );
              })}
            </div>
          </div>
        </div>
        
        {/* Scrollable content */}
        <div className="gantt-scroll overflow-x-auto">
          <div className="min-w-[800px]">
            {/* Pool rows */}
            <div className="stagger-children">
              {pools.map((pool, poolIdx) => {
                const poolSessions = deduplicatedData.filter(d => d.bad === pool);
                if (poolSessions.length === 0) return null;
                
                const rowMap = assignRows(poolSessions);
                const maxRows = Math.max(...Array.from(rowMap.values()), 0) + 1;
                const barHeight = 40;
                const rowSpacing = 4;
                const rowHeight = maxRows * (barHeight + rowSpacing) + 16;
                const poolColor = getPoolColor(pool, poolIdx);
                
                return (
                  <div 
                    key={pool} 
                    className="flex border-b border-base-200 last:border-0 hover:bg-base-50 transition-colors"
                  >
                    {/* Pool name - sticky */}
                    <div 
                      className="w-36 md:w-44 flex-shrink-0 p-3 font-bold text-sm flex items-start gap-2 sticky left-0 bg-base-100 z-10 border-r border-base-200"
                      style={{ minHeight: `${rowHeight}px` }}
                    >
                      <div 
                        className="w-3 h-3 rounded-full flex-shrink-0 mt-1"
                        style={{ backgroundColor: poolColor }}
                      ></div>
                      <span className="leading-tight">{pool}</span>
                    </div>
                    
                    {/* Time bars container */}
                    <div 
                      className="flex-1 relative"
                      style={{ height: `${rowHeight}px` }}
                    >
                      {/* Vertical grid lines */}
                      {hourMarkers.map(hour => (
                        <div
                          key={hour}
                          className="absolute top-0 bottom-0 w-px bg-base-200"
                          style={{ left: `${((hour - MIN_TIME) / TIME_RANGE) * 100}%` }}
                        ></div>
                      ))}
                      
                      {/* Session bars */}
                      {poolSessions.map((session) => {
                        const left = getBarPosition(session.start);
                        const width = getBarWidth(session.start, session.end);
                        const row = rowMap.get(session.id) || 0;
                        const top = row * (barHeight + rowSpacing) + 8;
                        const isNow = isSessionNow(session);
                        const category = getActivityCategory(session.activity);
                        const hasNote = hasImportantNote(session);
                        
                        // Determine color based on category and context
                        let color = isSingleDay 
                          ? poolColor
                          : (DAY_COLORS[session.dag] || poolColor);
                        
                        // Non-swimming activities get muted styling
                        const isOther = category === 'other';
                        if (isOther) {
                          color = '#9CA3AF'; // Light gray for non-swimming
                        }
                        
                        return (
                          <div
                            key={session.id}
                            className={`session-bar absolute rounded-lg flex items-center text-white text-xs font-bold cursor-pointer border-2 hover:scale-105 hover:z-20 active:scale-95 transition-transform ${
                              isNow ? 'ring-2 ring-success ring-offset-2' : ''
                            } ${isOther ? 'border-dashed border-gray-400' : 'border-white/40'} ${hasNote ? 'brightness-90' : ''}`}
                            style={{
                              left: `${left}%`,
                              width: `${Math.max(width, 3)}%`,
                              top: `${top}px`,
                              height: `${barHeight}px`,
                              backgroundColor: color,
                            }}
                            onMouseEnter={(e) => {
                              const rect = e.currentTarget.getBoundingClientRect();
                              setTooltip({
                                session,
                                x: rect.left + rect.width / 2,
                                y: rect.bottom,
                                color,
                              });
                            }}
                            onMouseLeave={() => setTooltip(null)}
                            onClick={() => {
                              setTooltip(null);
                              onSessionClick(session, color);
                            }}
                          >
                            {/* Content on bar */}
                            <div className="flex items-center justify-center w-full px-2 overflow-hidden gap-1">
                              {isOther && (
                                <span className="text-[10px] opacity-80 flex-shrink-0">●</span>
                              )}
                              <span className="truncate drop-shadow-md">
                                {formatTime(session.start)}-{formatTime(session.end)}
                              </span>
                              {hasNote && (
                                <span className="text-yellow-200 flex-shrink-0" title={session.extra}>*</span>
                              )}
                            </div>
                          </div>
                        );
                      })}
                    </div>
                  </div>
                );
              })}
            </div>
          </div>
        </div>
        
        {/* Legend */}
        <div className="p-4 bg-base-200/50 border-t border-base-200">
          <div className="space-y-3">
            {/* Pool/Day colors */}
            {isSingleDay ? (
              <div className="flex flex-wrap items-center gap-4 text-sm">
                <span className="text-base-content/60 font-semibold">Zwembaden:</span>
                {pools.map((pool, idx) => (
                  <div key={pool} className="flex items-center gap-2">
                    <div 
                      className="w-4 h-4 rounded shadow-sm"
                      style={{ backgroundColor: getPoolColor(pool, idx) }}
                    ></div>
                    <span className="text-base-content/80 font-medium">{pool}</span>
                  </div>
                ))}
              </div>
            ) : (
              <div className="flex flex-wrap items-center gap-4 text-sm">
                <span className="text-base-content/60 font-semibold">Dagen:</span>
                {Object.entries(DAY_COLORS).map(([day, color]) => (
                  <div key={day} className="flex items-center gap-2">
                    <div 
                      className="w-4 h-4 rounded shadow-sm"
                      style={{ backgroundColor: color }}
                    ></div>
                    <span className="text-base-content/80 font-medium">{DAY_ABBREVIATIONS[day]}</span>
                  </div>
                ))}
              </div>
            )}
            
            {/* Special indicators */}
            <div className="flex flex-wrap items-center gap-4 text-xs border-t border-base-300 pt-3">
              <div className="flex items-center gap-2">
                <div className="w-4 h-4 rounded bg-gray-400 border border-dashed border-gray-500"></div>
                <span className="text-base-content/60">Niet-zwemmen (squash etc.)</span>
              </div>
              <div className="flex items-center gap-2">
                <span className="text-yellow-500 font-bold">*</span>
                <span className="text-base-content/60">Let op / bijzonderheid</span>
              </div>
            </div>
          </div>
        </div>
      </div>
      
      {/* Tooltip */}
      {tooltip && (
        <Tooltip
          session={tooltip.session}
          x={tooltip.x}
          y={tooltip.y}
          poolColor={tooltip.color}
        />
      )}
    </>
  );
};

// ============================================================================
// Calendar View Component - Sessions span their actual duration
// ============================================================================

interface CalendarViewProps {
  data: SwimmingSession[];
  selectedDay: string;
  onSessionClick: (session: SwimmingSession, color: string) => void;
}

const CalendarView: React.FC<CalendarViewProps> = ({ data, selectedDay, onSessionClick }) => {
  // Filter out "Gesloten" sessions - they're confusing in calendar view
  const deduplicatedData = useMemo(() => 
    deduplicateSessions(data).filter(session => getActivityCategory(session.activity) !== 'closed'), 
    [data]);
  const pools = useMemo(() => Array.from(new Set(deduplicatedData.map(d => d.bad))).sort(), [deduplicatedData]);
  
  const hourSlots = Array.from({ length: MAX_TIME - MIN_TIME }, (_, i) => MIN_TIME + i);
  const currentTime = useCurrentTime();
  const today = getTodayDutch();
  const showNowLine = (selectedDay === 'Today' || selectedDay === today) && currentTime >= MIN_TIME && currentTime < MAX_TIME;
  
  const HOUR_HEIGHT = 60; // Height in pixels per hour
  
  // Assign horizontal positions for overlapping sessions in a pool
  const getSessionLayout = (pool: string) => {
    const poolSessions = deduplicatedData.filter(s => s.bad === pool);
    const layout = new Map<number, { column: number; totalColumns: number }>();
    
    // Sort by start time
    const sorted = [...poolSessions].sort((a, b) => a.start - b.start);
    
    // Group overlapping sessions
    sorted.forEach(session => {
      const overlapping = sorted.filter(s => 
        s.id !== session.id && 
        s.start < session.end && 
        s.end > session.start
      );
      
      // Find used columns
      const usedColumns = new Set<number>();
      overlapping.forEach(s => {
        const sLayout = layout.get(s.id);
        if (sLayout) usedColumns.add(sLayout.column);
      });
      
      // Find first available column
      let column = 0;
      while (usedColumns.has(column)) column++;
      
      const totalColumns = Math.max(column + 1, ...overlapping.map(s => {
        const l = layout.get(s.id);
        return l ? l.totalColumns : 1;
      }));
      
      layout.set(session.id, { column, totalColumns });
      
      // Update totalColumns for all overlapping sessions
      overlapping.forEach(s => {
        const l = layout.get(s.id);
        if (l && l.totalColumns < totalColumns) {
          layout.set(s.id, { ...l, totalColumns });
        }
      });
    });
    
    return layout;
  };
  
  if (pools.length === 0) return null;
  
  return (
    <div className="bg-base-100 rounded-xl shadow-lg overflow-hidden border border-base-300">
      {/* Header with pool names */}
      <div className="sticky top-0 z-20 bg-base-100 border-b-2 border-primary/20">
        <div className="flex">
          <div className="w-16 flex-shrink-0 p-3 bg-base-200 font-bold text-xs text-base-content/70">
            Tijd
          </div>
          {pools.map((pool, idx) => (
            <div 
              key={pool}
              className="flex-1 p-3 font-bold text-sm text-center border-l border-base-200 min-w-[140px]"
              style={{ backgroundColor: `${getPoolColor(pool, idx)}10` }}
            >
              <div 
                className="w-3 h-3 rounded-full mx-auto mb-1"
                style={{ backgroundColor: getPoolColor(pool, idx) }}
              ></div>
              <span className="text-xs leading-tight">{pool}</span>
            </div>
          ))}
        </div>
      </div>
      
      {/* Time grid with positioned sessions */}
      <div className="relative">
        {/* Now indicator */}
        {showNowLine && (
          <div 
            className="absolute left-0 right-0 h-0.5 bg-error now-indicator z-20"
            style={{ top: `${(currentTime - MIN_TIME) * HOUR_HEIGHT}px` }}
          >
            <div className="absolute left-2 -top-2.5 bg-error text-white text-xs font-bold px-2 py-0.5 rounded-full shadow-lg">
              Nu {formatTime(currentTime)}
            </div>
          </div>
        )}
        
        {/* Hour rows - just for grid lines */}
        {hourSlots.map(hour => (
          <div 
            key={hour} 
            className="flex border-b border-base-200 last:border-0"
            style={{ height: `${HOUR_HEIGHT}px` }}
          >
            {/* Time label */}
            <div className="w-16 flex-shrink-0 p-2 text-xs font-bold text-base-content/60 bg-base-50 border-r border-base-200">
              {hour}:00
            </div>
            
            {/* Pool columns - empty grid cells */}
            {pools.map((pool) => (
              <div 
                key={pool}
                className="flex-1 border-l border-base-200 min-w-[140px] bg-base-50/30"
              />
            ))}
          </div>
        ))}
        
        {/* Positioned session blocks - overlay on grid */}
        {pools.map((pool, poolIdx) => {
          const poolColor = getPoolColor(pool, poolIdx);
          const poolSessions = deduplicatedData.filter(s => s.bad === pool);
          const layout = getSessionLayout(pool);
          
          return poolSessions.map(session => {
            const sessionLayout = layout.get(session.id) || { column: 0, totalColumns: 1 };
            const isNow = isSessionNow(session);
            const category = getActivityCategory(session.activity);
            const isClosed = category === 'closed';
            const isOther = category === 'other';
            const hasNote = hasImportantNote(session);
            
            let bgColor = poolColor;
            if (isClosed) bgColor = '#6B7280';
            else if (isOther) bgColor = '#9CA3AF';
            
            // Calculate position
            const topPosition = (session.start - MIN_TIME) * HOUR_HEIGHT;
            const height = Math.max((session.end - session.start) * HOUR_HEIGHT - 4, 24); // Min 24px height
            
            // Column width percentage within the pool cell
            const widthPercent = 100 / sessionLayout.totalColumns;
            const leftPercent = sessionLayout.column * widthPercent;
            
            return (
              <div
                key={session.id}
                className={`absolute rounded-lg text-white text-xs cursor-pointer hover:z-30 hover:scale-[1.02] active:scale-[0.98] transition-transform shadow-sm ${
                  isNow ? 'ring-2 ring-success ring-offset-1 z-10' : ''
                } ${isClosed ? 'opacity-60' : ''} ${isOther ? 'border border-dashed border-gray-400' : 'border border-white/30'}`}
                style={{ 
                  top: `${topPosition}px`,
                  height: `${height}px`,
                  // Position within the pool column
                  left: `calc(64px + (100% - 64px) * ${poolIdx / pools.length} + (100% - 64px) / ${pools.length} * ${leftPercent / 100} + 4px)`,
                  width: `calc((100% - 64px) / ${pools.length} * ${widthPercent / 100} - 8px)`,
                  backgroundColor: bgColor,
                  backgroundImage: isClosed ? 'repeating-linear-gradient(45deg, transparent, transparent 3px, rgba(255,255,255,0.1) 3px, rgba(255,255,255,0.1) 6px)' : 'none',
                }}
                onClick={() => onSessionClick(session, bgColor)}
              >
                <div className="p-1.5 h-full flex flex-col overflow-hidden">
                  <div className="font-bold flex items-center gap-1 flex-shrink-0">
                    {isClosed && <X size={10} />}
                    {isClosed ? 'Gesloten' : `${formatTime(session.start)}-${formatTime(session.end)}`}
                    {hasNote && !isClosed && <span className="text-yellow-200">*</span>}
                  </div>
                  {!isClosed && height > 40 && (
                    <div className="opacity-80 truncate flex items-center gap-1 text-[10px] mt-0.5">
                      {isOther && <span className="opacity-60">●</span>}
                      {session.activity}
                    </div>
                  )}
                </div>
              </div>
            );
          });
        })}
      </div>
    </div>
  );
};

// ============================================================================
// List View Component
// ============================================================================

interface ListViewProps {
  data: SwimmingSession[];
  onSessionClick: (session: SwimmingSession, color: string) => void;
}

const ListView: React.FC<ListViewProps> = ({ data, onSessionClick }) => {
  const deduplicatedData = useMemo(() => deduplicateSessions(data), [data]);
  const sortedData = useMemo(() => 
    [...deduplicatedData].sort((a, b) => a.start - b.start),
    [deduplicatedData]
  );
  
  return (
    <div className="grid grid-cols-1 gap-4 md:grid-cols-2 lg:grid-cols-3 stagger-children">
      {sortedData.map((session, idx) => {
        const isNow = isSessionNow(session);
        const category = getActivityCategory(session.activity);
        const isClosed = category === 'closed';
        const isOther = category === 'other';
        const hasNote = hasImportantNote(session);
        
        let poolColor = getPoolColor(session.bad, idx);
        if (isClosed) poolColor = '#6B7280';
        else if (isOther) poolColor = '#9CA3AF';
        
        return (
          <div 
            key={session.id} 
            className={`card bg-base-100 shadow-lg hover:shadow-xl transition-all border-l-4 cursor-pointer active:scale-[0.98] ${
              isNow ? 'ring-2 ring-success' : ''
            } ${isClosed ? 'opacity-70' : ''} ${isOther ? 'border-dashed' : ''}`}
            style={{ borderLeftColor: poolColor }}
            onClick={() => onSessionClick(session, poolColor)}
          >
            <div className="card-body p-4">
              <div className="flex justify-between items-start gap-2">
                <h2 className="card-title text-base font-bold">{session.bad}</h2>
                <div className="flex gap-1 flex-wrap justify-end">
                  {isClosed && (
                    <span className="badge badge-ghost badge-sm">Gesloten</span>
                  )}
                  {isOther && (
                    <span className="badge badge-ghost badge-sm">Overig</span>
                  )}
                  {isNow && (
                    <span className="badge badge-success badge-sm nu-open-badge">Nu open</span>
                  )}
                  <span className="badge badge-ghost badge-sm">{DAY_ABBREVIATIONS[session.dag]}</span>
                </div>
              </div>
              
              {!isClosed && (
                <div className="flex items-center gap-2 text-primary font-bold">
                  <Clock size={16} />
                  <span>{formatTime(session.start)} - {formatTime(session.end)}</span>
                  {hasNote && <span className="text-warning">*</span>}
                </div>
              )}
              
              <div className="mt-2 pt-2 border-t border-base-200">
                <p className="font-semibold flex items-center gap-1">
                  {isOther && <span className="text-gray-400">●</span>}
                  {session.activity}
                </p>
                {session.extra && (
                  <div className={`mt-2 text-sm p-2 rounded ${hasNote ? 'bg-warning/10 border border-warning/30' : 'bg-base-200'}`}>
                    {hasNote && <AlertTriangle size={12} className="inline mr-1 text-warning" />}
                    <span className="text-base-content/70">{session.extra}</span>
                  </div>
                )}
              </div>
              
              <div className="mt-3 flex items-center justify-between">
                <span className="text-xs text-accent font-semibold">Klik voor meer →</span>
                <ExternalLink size={14} className="text-base-content/40" />
              </div>
            </div>
          </div>
        );
      })}
    </div>
  );
};

// ============================================================================
// Empty State Component
// ============================================================================

const EmptyState: React.FC = () => (
  <div className="flex flex-col items-center justify-center p-12 text-center bg-base-100 rounded-xl shadow-lg">
    <div className="bg-base-200 p-6 rounded-full mb-4">
      <Waves size={48} className="text-secondary" />
    </div>
    <h3 className="text-xl font-bold mb-2">Geen zwemtijden gevonden</h3>
    <p className="text-base-content/60 max-w-md">
      Er zijn geen sessies die overeenkomen met je filters. Probeer een andere dag of activiteit te selecteren.
    </p>
  </div>
);

// ============================================================================
// Main App Component
// ============================================================================

const App: React.FC = () => {
  const [data, setData] = useState<SwimmingSession[]>([]);
  const [filteredData, setFilteredData] = useState<SwimmingSession[]>([]);
  const [metadata, setMetadata] = useState<Metadata | null>(null);
  const [loading, setLoading] = useState(true);
  // Default: calendar on mobile, timeline on desktop
  const [viewMode, setViewMode] = useState<'timeline' | 'calendar' | 'list'>(() => {
    if (typeof window !== 'undefined' && window.innerWidth < 768) {
      return 'calendar';
    }
    return 'timeline';
  });
  const [filtersExpanded, setFiltersExpanded] = useState(true);
  
  
  // Modals
  const [selectedSession, setSelectedSession] = useState<{ session: SwimmingSession; color: string } | null>(null);
  const [showAbout, setShowAbout] = useState(false);
  
  // Filters
  const [searchTerm, setSearchTerm] = useState('');
  const [selectedPool, setSelectedPool] = useState('All Pools');
  const [selectedWeek, setSelectedWeek] = useState<'this' | 'next'>('this');
  const [selectedDay, setSelectedDay] = useState('Today');
  const [selectedActivity, setSelectedActivity] = useState('Banenzwemmen');

  // Load data and metadata
  useEffect(() => {
    // Use import.meta.env.BASE_URL which Vite sets based on config
    const baseUrl = import.meta.env.BASE_URL || '/';
    Promise.all([
      fetch(`${baseUrl}data.json`).then(res => res.json()),
      fetch(`${baseUrl}metadata.json`).then(res => res.json()).catch(() => null)
    ])
      .then(([jsonData, jsonMeta]) => {
        setData(jsonData as SwimmingSession[]);
        setMetadata(jsonMeta as Metadata);
        setLoading(false);
      })
      .catch(err => {
        console.error("Error loading data:", err);
        setLoading(false);
      });
  }, []);
  
  // Session click handler
  const handleSessionClick = (session: SwimmingSession, color: string) => {
    setSelectedSession({ session, color });
  };

  // Filter data
  useEffect(() => {
    let result = data;
    const today = getTodayDutch();
    const todayDateStr = formatDateISO(new Date());
    const weekOffset = selectedWeek === 'this' ? 0 : 1;
    const weekInfo = getWeekInfo(weekOffset);

    if (selectedPool !== 'All Pools') {
      result = result.filter(item => item.bad === selectedPool);
    }

    // Week-based filtering
    if (selectedDay === 'All Days') {
      // Filter to selected week
      result = result.filter(item => {
        if (!item.date) return false;
        const itemDate = new Date(item.date);
        return itemDate >= weekInfo.start && itemDate <= weekInfo.end;
      });
    } else if (selectedDay === 'Today') {
      // Filter to today
      result = result.filter(item => item.date === todayDateStr || item.dag === today);
    } else {
      // selectedDay is now a date string (YYYY-MM-DD)
      result = result.filter(item => item.date === selectedDay);
    }

    if (selectedActivity !== 'All Activities') {
      // Special handling for Aqua category (matches Aqua*, Float*, Aquajoggen)
      if (selectedActivity === 'Aqua') {
        result = result.filter(item => {
          const lower = item.activity.toLowerCase();
          return lower.includes('aqua') || lower.includes('float');
        });
      } else {
        result = result.filter(item => item.activity.toLowerCase().includes(selectedActivity.toLowerCase()));
      }
    }

    if (searchTerm) {
      const term = searchTerm.toLowerCase();
      result = result.filter(item => 
        item.bad.toLowerCase().includes(term) ||
        item.activity.toLowerCase().includes(term) ||
        item.extra.toLowerCase().includes(term)
      );
    }

    setFilteredData(result);
  }, [data, searchTerm, selectedPool, selectedWeek, selectedDay, selectedActivity]);

  // Derived state
  const pools = useMemo(() => ['All Pools', ...new Set(data.map(item => item.bad))], [data]);
  const availableDates = useMemo(() => [...new Set(data.map(item => item.date).filter((d): d is string => !!d))], [data]);
  const availableActivities = useMemo(() => [...new Set(data.map(item => item.activity))], [data]);

  // Jump to now handler
  const handleJumpToNow = () => {
    setSelectedWeek('this');
    setSelectedDay('Today');
  };

  if (loading) {
    return (
      <div className="min-h-screen bg-water-gradient p-4 md:p-8">
        <header className="mb-8 flex items-center gap-3">
          <WaveXXXLogo size="lg" />
          <div>
            <h1 className="text-xl md:text-2xl font-black text-primary">ZWEMSTERDAM</h1>
            <p className="text-xs text-base-content/50">Alle zwemtijden op één plek</p>
          </div>
        </header>
        <LoadingSkeleton />
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-water-gradient pb-20 md:pb-8">
      {/* Header */}
      <header className="sticky top-0 z-30 bg-base-100/95 backdrop-blur-lg shadow-lg border-b border-base-200">
        <div className="max-w-7xl mx-auto px-4 md:px-6 py-3">
          {/* Single row layout with proper alignment */}
          <div className="flex items-center gap-3 md:gap-6">
            {/* Logo + Title - left */}
            <div className="flex items-center gap-2 md:gap-3 flex-shrink-0">
              <WaveXXXLogo size="md" />
              <div>
                <h1 className="text-base sm:text-lg md:text-xl font-black text-primary tracking-tight leading-none">
                  ZWEMSTERDAM
                </h1>
                <p className="text-[9px] sm:text-[10px] md:text-xs text-base-content/50 font-medium leading-tight mt-0.5">
                  Alle zwemtijden op één plek
                </p>
              </div>
            </div>
            
            {/* View toggles - center (desktop only) */}
            <div className="hidden md:flex items-center justify-center flex-1">
              <div className="inline-flex items-center bg-base-200/80 p-1 rounded-2xl shadow-inner border border-base-300/50">
                <button
                  onClick={() => setViewMode('timeline')}
                  className={`flex items-center gap-2 px-4 py-2 rounded-xl text-sm font-semibold transition-all duration-200 ${
                    viewMode === 'timeline' 
                      ? 'bg-white text-primary shadow-md' 
                      : 'text-base-content/60 hover:text-base-content hover:bg-base-100/50'
                  }`}
                >
                  <BarChart3 size={16} />
                  <span>Tijdlijn</span>
                </button>
                <button
                  onClick={() => setViewMode('calendar')}
                  className={`flex items-center gap-2 px-4 py-2 rounded-xl text-sm font-semibold transition-all duration-200 ${
                    viewMode === 'calendar' 
                      ? 'bg-white text-primary shadow-md' 
                      : 'text-base-content/60 hover:text-base-content hover:bg-base-100/50'
                  }`}
                >
                  <Calendar size={16} />
                  <span>Kalender</span>
                </button>
                <button
                  onClick={() => setViewMode('list')}
                  className={`flex items-center gap-2 px-4 py-2 rounded-xl text-sm font-semibold transition-all duration-200 ${
                    viewMode === 'list' 
                      ? 'bg-white text-primary shadow-md' 
                      : 'text-base-content/60 hover:text-base-content hover:bg-base-100/50'
                  }`}
                >
                  <List size={16} />
                  <span>Lijst</span>
                </button>
              </div>
            </div>
            
            {/* Right side: Search + Theme + Info (desktop) */}
            <div className="hidden md:flex items-center gap-2 flex-shrink-0">
              <div className="inline-flex items-center bg-base-200/80 p-1 rounded-2xl shadow-inner border border-base-300/50">
                <div className="relative">
                  <input 
                    type="text"
                    className="w-44 lg:w-52 pl-9 pr-3 py-2 text-sm bg-transparent rounded-xl focus:outline-none focus:bg-base-100 focus:shadow-md transition-all placeholder:text-base-content/40" 
                    placeholder="Zoeken..." 
                    value={searchTerm}
                    onChange={(e) => setSearchTerm(e.target.value)}
                  />
                  <Search size={14} className="absolute left-3 top-1/2 -translate-y-1/2 text-base-content/40" />
                </div>
              </div>
              <button
                onClick={() => setShowAbout(true)}
                className="p-2 rounded-xl hover:bg-base-200 transition-colors text-base-content/60 hover:text-primary"
                title="Over Zwemsterdam"
              >
                <Info size={18} />
              </button>
            </div>
            
            {/* Mobile search (shows inline on mobile, right-aligned) */}
            <div className="ml-auto md:hidden">
              <div className="inline-flex items-center bg-base-200/80 p-1 rounded-2xl shadow-inner border border-base-300/50">
                <div className="relative">
                  <input
                    type="text"
                    className="w-36 pl-9 pr-3 py-1.5 text-sm bg-transparent rounded-xl focus:outline-none focus:bg-white focus:shadow-md focus:w-48 transition-all placeholder:text-base-content/40"
                    placeholder="Zoeken..."
                    value={searchTerm}
                    onChange={(e) => setSearchTerm(e.target.value)}
                  />
                  <Search size={14} className="absolute left-3 top-1/2 -translate-y-1/2 text-base-content/40" />
                </div>
              </div>
            </div>
          </div>
        </div>
      </header>
      
      {/* Filters */}
      <div className="px-4 md:px-6 py-4">
        <div className="bg-base-100 rounded-xl shadow-lg border border-base-300 overflow-hidden">
          {/* Collapsible header - visible on all screen sizes */}
          <button
            onClick={() => setFiltersExpanded(!filtersExpanded)}
            className="w-full flex items-center justify-between p-4 hover:bg-base-200/50 transition-colors"
          >
            <span className="font-bold flex items-center gap-2">
              <MapPin size={16} className="text-primary" />
              Filters
              {!filtersExpanded && (
                <span className="text-sm font-normal text-base-content/50 ml-2">
                  ({selectedDay === 'Today' ? 'Vandaag' : selectedDay === 'All Days' ? 'Hele week' : selectedDay} • {selectedActivity === 'All Activities' ? 'Alle activiteiten' : selectedActivity})
                </span>
              )}
            </span>
            <div className="flex items-center gap-2">
              <span className="text-xs text-base-content/50 hidden sm:inline">
                {filtersExpanded ? 'Verbergen' : 'Tonen'}
              </span>
              {filtersExpanded ? <ChevronUp size={20} /> : <ChevronDown size={20} />}
            </div>
          </button>

          {/* Filter content */}
          <div className={`p-4 pt-0 space-y-4 ${filtersExpanded ? 'block' : 'hidden'}`}>
            {/* Week selector - prominent display */}
            <WeekSelector 
              selectedWeek={selectedWeek}
              onSelectWeek={(week) => {
                setSelectedWeek(week);
                // Reset day selection when switching weeks
                if (week === 'next' && selectedDay === 'Today') {
                  setSelectedDay('All Days');
                }
              }}
              availableDates={availableDates}
            />
            
            {/* Day pills */}
            <div>
              <label className="text-xs font-bold text-base-content/60 uppercase tracking-wide mb-2 block">
                Dag
              </label>
              <DayPills 
                selectedDay={selectedDay}
                onSelectDay={setSelectedDay}
                selectedWeek={selectedWeek}
                availableDates={availableDates}
              />
            </div>
            
            {/* Activity chips */}
            <div>
              <label className="text-xs font-bold text-base-content/60 uppercase tracking-wide mb-2 block">
                Activiteit
              </label>
              <ActivityChips
                selectedActivity={selectedActivity}
                onSelectActivity={setSelectedActivity}
                availableActivities={availableActivities}
              />
            </div>
            
            {/* Pool filter */}
            <div>
              <label className="text-xs font-bold text-base-content/60 uppercase tracking-wide mb-2 block">
                Zwembad
              </label>
              <div className="relative max-w-xs">
                <select 
                  className="w-full px-4 py-2 text-sm bg-base-200 rounded-xl border-0 appearance-none cursor-pointer focus:outline-none focus:ring-2 focus:ring-primary focus:bg-base-100 transition-colors pr-10"
                  value={selectedPool}
                  onChange={(e) => setSelectedPool(e.target.value)}
                >
                  {pools.map(pool => (
                    <option key={pool} value={pool}>
                      {pool === 'All Pools' ? 'Alle zwembaden' : pool}
                    </option>
                  ))}
                </select>
                <ChevronDown size={16} className="absolute right-3 top-1/2 -translate-y-1/2 text-base-content/40 pointer-events-none" />
              </div>
            </div>
          </div>
        </div>
      </div>
      
      {/* Main content */}
      <main className="px-4 md:px-6">
        {filteredData.length === 0 ? (
          <EmptyState />
        ) : viewMode === 'timeline' ? (
          <GanttView data={filteredData} selectedDay={selectedDay} onSessionClick={handleSessionClick} />
        ) : viewMode === 'calendar' ? (
          <CalendarView data={filteredData} selectedDay={selectedDay} onSessionClick={handleSessionClick} />
        ) : (
          <ListView data={filteredData} onSessionClick={handleSessionClick} />
        )}
      </main>
      
      {/* Jump to Now FAB */}
      {selectedDay !== 'Today' && (
        <button
          onClick={handleJumpToNow}
          className="fixed bottom-24 md:bottom-8 right-4 md:right-8 btn btn-primary btn-circle shadow-lg z-40"
          title="Spring naar nu"
        >
          <Navigation size={20} />
        </button>
      )}
      
      {/* Mobile bottom nav */}
      <nav className="fixed bottom-0 left-0 right-0 bg-base-100 border-t border-base-300 shadow-lg md:hidden mobile-bottom-nav z-30">
        <div className="flex justify-around py-2">
          <button
            onClick={() => setViewMode('timeline')}
            className={`flex flex-col items-center p-2 rounded-lg min-w-[48px] ${
              viewMode === 'timeline' ? 'text-primary bg-primary/10' : 'text-base-content/60'
            }`}
          >
            <BarChart3 size={20} />
            <span className="text-[10px] mt-1 font-medium">Tijdlijn</span>
          </button>
          <button
            onClick={() => setViewMode('calendar')}
            className={`flex flex-col items-center p-2 rounded-lg min-w-[48px] ${
              viewMode === 'calendar' ? 'text-primary bg-primary/10' : 'text-base-content/60'
            }`}
          >
            <Calendar size={20} />
            <span className="text-[10px] mt-1 font-medium">Kalender</span>
          </button>
          <button
            onClick={() => setViewMode('list')}
            className={`flex flex-col items-center p-2 rounded-lg min-w-[48px] ${
              viewMode === 'list' ? 'text-primary bg-primary/10' : 'text-base-content/60'
            }`}
          >
            <List size={20} />
            <span className="text-[10px] mt-1 font-medium">Lijst</span>
          </button>
          <button
            onClick={() => setShowAbout(true)}
            className="flex flex-col items-center p-2 rounded-lg min-w-[48px] text-base-content/60"
          >
            <Info size={20} />
            <span className="text-[10px] mt-1 font-medium">Info</span>
          </button>
        </div>
      </nav>
      
      {/* Footer */}
      <footer className="mt-12 px-4 md:px-6 py-8 border-t border-base-300 bg-base-200/50">
        <div className="max-w-4xl mx-auto">
          {/* Disclaimer */}
          <div className="flex items-start gap-3 p-4 bg-warning/10 rounded-xl mb-6">
            <AlertTriangle size={20} className="text-warning flex-shrink-0 mt-0.5" />
            <div className="text-sm text-base-content/70">
              <strong className="text-base-content">Disclaimer:</strong> Zwemsterdam is een onofficieel hulpmiddel. 
              Controleer altijd de <button onClick={() => setShowAbout(true)} className="text-secondary hover:underline font-semibold">officiële websites</button> voor de meest actuele zwemtijden — 
              <em className="text-warning-content font-medium">vooral tijdens feestdagen en schoolvakanties, wanneer openingstijden vaak afwijken.</em>
            </div>
          </div>
          
          <div className="flex flex-col md:flex-row items-center justify-between gap-4 text-sm text-base-content/60">
            <div className="flex items-center gap-4">
              <div>
                <p className="font-bold text-base-content">Zwemsterdam</p>
                <p className="text-xs text-base-content/50">Gemaakt door Fabio Votta</p>
              </div>
              <a
                href="https://github.com/favstats"
                target="_blank"
                rel="noopener noreferrer"
                className="btn btn-ghost btn-xs gap-1"
              >
                <Github size={14} />
                @favstats
              </a>
              <a
                href="https://www.buymeacoffee.com/favstats"
                target="_blank"
                rel="noopener noreferrer"
                className="btn btn-ghost btn-xs gap-1 text-amber-600 hover:text-amber-700"
              >
                <Coffee size={14} />
                Doneren
              </a>
              <button
                onClick={() => setShowAbout(true)}
                className="text-secondary hover:underline flex items-center gap-1"
              >
                <Info size={14} />
                Over & Bronnen
              </button>
            </div>
            
            {metadata && (
              <div className="flex items-center gap-2 text-xs">
                <Database size={14} />
                <span>Laatst bijgewerkt: {formatLastUpdated(metadata.lastUpdated)}</span>
              </div>
            )}
          </div>
        </div>
      </footer>
      
      {/* Session Detail Modal */}
      {selectedSession && (
        <SessionModal
          session={selectedSession.session}
          poolColor={selectedSession.color}
          onClose={() => setSelectedSession(null)}
        />
      )}
      
      {/* About Modal */}
      {showAbout && (
        <AboutModal
          metadata={metadata}
          onClose={() => setShowAbout(false)}
        />
      )}
    </div>
  );
};

export default App;

