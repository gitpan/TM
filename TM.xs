#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include <topicmaps.h>
#include <tmmodel.h>

static TM tmhdl;

/* user data for VIEW callbacks */
struct user_data
{
	SV *user_data_ref;	/* Perl app's user data */
	SV *start;		/* Perl app's start callback */
	SV *end;		/* Perl app's end callback */
};

/* C callback for VIEW start events. Dispatches to the Perl
 * callback passed in via user data
 */
static int vstart(void* ud, const char *name, void **atts)
{
	SV *user_data_ref;
	SV *start;
	HV *perl_atts;	/* hash for passing atts to Perl callback */
	int i;
        struct user_data *up = (struct user_data*)ud;
	dSP ;
	start = up->start;
	user_data_ref = up->user_data_ref;
	perl_atts = newHV();

	/* For each (attribute,value,value type) tuple construct the
	 * corresponding entry in the hash 'perl_atts'.
	 */

	for (i = 0; atts && atts[i]; i += 3)
	{
		char *key;
		void *value;
		TMValueType vtype;

		key = (char*)atts[i];
		value = atts[i+1];
		vtype = (TMValueType)atts[i+2];
		if(strcmp(vtype->name,"Topic") == 0)
		{
			hv_store(perl_atts,
				key, strlen(key), newSViv( (int)value ) , 0);
		}
		else if(strcmp(vtype->name,"Text") == 0)
		{
			if(value)
				hv_store(perl_atts,
					key, strlen(key), newSVpv((char*)value,0) , 0);
		}
		else if(strcmp(vtype->name,"TextSet") == 0)
		{
			if(value)
			{
			AV *perl_set;
			TMList lp;
			perl_set = newAV();
			for(lp= (TMList)value; lp; lp=lp->next)
			{
				av_push(perl_set,newSVpv((char*)lp->content,0));
			}
			hv_store(perl_atts,
				key, strlen(key), newRV_inc((SV*)perl_set),0);
			}
		}
		else
		{
			warn("unknown value type '%s' in view event "
			     "attributes, cannot convert to Perl value. "
			     "Skiping attribute!", vtype->name );
		}
	}
	ENTER ;
	SAVETMPS ;
	PUSHMARK(SP) ;
	XPUSHs(sv_2mortal(newSVsv(user_data_ref)));
	XPUSHs(sv_2mortal(newSVpv(name, 0)));
	XPUSHs(sv_2mortal( newRV_inc( (SV*) perl_atts)) );
	PUTBACK ;

	call_sv(start,G_VOID);

	FREETMPS ;
	LEAVE ;
	return(0);
}
/* C callback for view end events. Dispatches to the Perl
 * callback passed in via user data
 */
static int vend(void* ud, const char *name)
{
	SV *user_data_ref;
	SV *end;
        struct user_data *up = (struct user_data*)ud;
	dSP ;
	end = up->end;
	user_data_ref = up->user_data_ref;
	ENTER ;
	SAVETMPS ;
	PUSHMARK(SP) ;
	XPUSHs(sv_2mortal(newSVsv(user_data_ref)));
	XPUSHs(sv_2mortal(newSVpv(name, 0)));
	PUTBACK ;

	call_sv(end,G_VOID);	/* call Perl callback */

	FREETMPS ;
	LEAVE ;
	return(0);
}
	
	

MODULE = TM		PACKAGE = TM::TopicMap

# Make sure that we have at least xsubpp version 1.922.
REQUIRE: 1.922

TMTopicMap
new(CLASS)
	char *CLASS
    PREINIT:
	TMTopicMap tm;
    CODE:
	tm = tm_topicmap_new(tmhdl);
	if( tm == NULL )
	{
		warn("unable to create TopicMap object");
		XSRETURN_UNDEF;
	}
	if( tm_topicmap_set_storage(tm,"MemStore") != TM_OK)
	{
		warn("unable to set storage, %s", tm_get_error(tmhdl) );
		XSRETURN_UNDEF;	
	}
	if( tm_topicmap_open(tm,NULL) != TM_OK)
	{
		warn("unable to open topic map, %s",
			tm_get_error(tmhdl));
		tm_topicmap_delete(&tm);
		XSRETURN_UNDEF;
	}
	RETVAL = tm;
    OUTPUT:
	RETVAL

void
DESTROY(self)
	TMTopicMap self
    CODE:
	tm_topicmap_close(self);	/* FIXME: return value */
	tm_topicmap_delete(&self);



int
require( self , val )
        TMTopicMap self
        char *val
    PREINIT:
	TMModel m;
	TMError e;
	int eins = 1;
    CODE:
	if(tm_topicmap_require_model(self,val) != TM_OK)
	{
		warn("unable to load model %s to topic map, %s", val,
			tm_get_error(tmhdl) );
		XSRETURN_UNDEF;
	}
	RETVAL = eins;
    OUTPUT:
	RETVAL


char*
get_error( self )
        TMTopicMap self
    PREINIT:
	char *s;
    CODE:
	s = (char*)tm_get_error(tmhdl);
	RETVAL = s;
    OUTPUT:
	RETVAL


void
dump( self )
        TMTopicMap self
    CODE:
	tm_topicmap_dump(self,stderr);



int
load_file( self , fname , pm_name, parse_type)
        TMTopicMap self
        char *fname
	char *pm_name
	char *parse_type
    PREINIT:
	int eins = 1;
	TMError e;
	int fd;
	struct stat stbuf;
	Omnivore o;
    CODE:
	if( (fd = open(fname,O_RDONLY,0)) < 0)
	{
		tm_set_error(tmhdl,"cannot open %s, %s\n", fname, strerror(errno));
		XSRETURN_UNDEF;
	}
	if( fstat(fd,&stbuf) < 0)
	{
		tm_set_error(tmhdl,"cannot stat %s, %s\n",
                                fname, strerror(errno));
		XSRETURN_UNDEF;
	}
	o = Omnivore_new(tmhdl);
	tm_omnivore_prepare(o,parse_type,pm_name,NULL,self);
	Omnivore_setDocUri(o,fname);
	for (;;)
	{
		char buf[4096];
		int len;
 
		if( (len = read(fd, buf, sizeof(buf))) < 0)
		{
			tm_set_error(tmhdl,"error reading %s, %s\n", fname, strerror(errno));
			XSRETURN_UNDEF;
		}
 
		if (! Omnivore_parse(o, buf, len, len == 0))
		{
			XSRETURN_UNDEF;
		}
		if (len == 0)
			break;
	}
	Omnivore_delete( &o );
	if( tm_topicmap_fully_merge(self) != TM_OK)
	{
		XSRETURN_UNDEF;
	}
	RETVAL=eins;
   OUTPUT:
	RETVAL

int
load_string( self , string , pm_name, parse_type)
        TMTopicMap self
        char *string
	char *pm_name
	char *parse_type
    PREINIT:
	int eins = 1;
	TMError e;
	Omnivore o;
    CODE:
	o = Omnivore_new(tmhdl);
	tm_omnivore_prepare(o,parse_type,pm_name,NULL,self);
	Omnivore_setDocUri(o,"fake://PARSED_FROM_CHUNK");
	if (! Omnivore_parse(o, string, strlen(string), 1 /* isFinal! */))
	{
		tm_set_error(tmhdl,"Parse error: %s\n", Omnivore_getErrorString(o) );
		Omnivore_delete( &o );
		XSRETURN_UNDEF;
	}
	Omnivore_delete( &o );
	if( tm_topicmap_fully_merge(self) != TM_OK)
	{
		XSRETURN_UNDEF;
	}
	RETVAL=eins;
   OUTPUT:
	RETVAL



void
query( self , user_data_ref, start, end, query )
        TMTopicMap self
	SV* user_data_ref
	SV* start
	SV* end
        char *query
    PREINIT:
	TMModel m;
	TMError e;
	struct user_data ud;
    CODE:
	ud.user_data_ref = user_data_ref;
	ud.start = start;
	ud.end = end;
	tm_topicmap_query(self,&ud,vstart,vend,query);

SV *
get_property(self,topic, fullname )
	TMTopicMap self
	int topic
	char *fullname
    PREINIT:
	char m[256];
	char *p;
	void *value;
	TMProperty prop;
	TMValueType vtype;
	TMError e;
    CODE:
	p = strstr(fullname,"::");
	assert(p);
	bzero(m,sizeof(m));
	strncpy(m,fullname, (p-fullname) );
	p += 2;  /* skip '::' */
	e = tm_topicmap_get_property(self, topic,m,p,&value,&prop);
	assert(e == TM_OK);
	
	if(!value)
		XSRETURN_UNDEF;
	vtype = prop->value_type;
	if(strcmp(vtype->name,"Topic") == 0)
	{
		RETVAL = newSViv( (int)value );
	}
	else if(strcmp(vtype->name,"Text") == 0)
	{
		RETVAL = newSVpv((char*)value,0);
	}
	else if(strcmp(vtype->name,"TextSet") == 0)
	{
		AV *perl_set;
		TMList lp;
		perl_set = newAV();
		for(lp= (TMList)value; lp; lp=lp->next)
		{
			av_push(perl_set, newSVpv((char*)lp->content, 0) );
		}
		RETVAL = newRV_inc( (SV*) perl_set);
	}
	else
	{
		warn("unknown value type");
		XSRETURN_UNDEF;
	}
    OUTPUT:
        RETVAL


SV *
get_topic(self,fullname,value )
	TMTopicMap self
	char *fullname
	void* value
    PREINIT:
	char m[256];
	char *p;
	TMProperty prop;
	TMTopic topic;
	TMError e;
    CODE:
	p = strstr(fullname,"::");
	assert(p);
	bzero(m,sizeof(m));
	strncpy(m,fullname, (p-fullname) );
	p += 2;  /* skip '::' */
	if( (e = tm_topicmap_get_topic(self, m,p,value,&topic,&prop)) != TM_OK)
	{
		/* FIXME: if prop, to_tring value and put in warning */
		warn("cannot get topic by %s, %s",fullname,
			tm_get_error(tmhdl));
		XSRETURN_UNDEF;
	}
	RETVAL = newSViv( (int)topic );
    OUTPUT:
        RETVAL




MODULE = TM			PACKAGE = TM


BOOT:
	if(tm_init(&tmhdl,"www-df",NULL) != TM_OK)
        {
                fprintf(stderr,"ERROR: %s\n",tm_get_error(tmhdl));
                exit(1);
        }

void
set_trace(mask)
	char *mask
        CODE:
        tm_set_trace_mask(mask);
