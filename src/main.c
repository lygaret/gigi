#define SDL_MAIN_USE_CALLBACKS 1  /* use the callbacks instead of main() */

#include <stdio.h>
#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
#include <uv.h>

static SDL_Window   *window = NULL;
static SDL_Renderer *renderer = NULL;
static SDL_Thread   *iothread = NULL;

static uv_rwlock_t  ioQuittingLock;
static bool         ioQuitting = false;

void ioRequestQuit() {
    uv_rwlock_wrlock(&ioQuittingLock);
    ioQuitting = true;
    uv_rwlock_wrunlock(&ioQuittingLock);
}

bool ioShouldQuit() {
    uv_rwlock_rdlock(&ioQuittingLock);
    const bool current = ioQuitting;
    uv_rwlock_rdunlock(&ioQuittingLock);

    return current;
}

// push an event from the SDL side

static uv_async_t   ioSDLEventAsync;  
static uv_rwlock_t  ioSDLEventLock;
static SDL_Event    ioSDLLatestEvent;
// TODO: should be atomic buffer of some kind
// since the async event isn't 1-1, but rather many-1, we need to be able to collect and handle multiple events

void ioPushSDLEvent(SDL_Event *event) {
    uv_rwlock_wrlock(&ioSDLEventLock);
    memcpy(&ioSDLLatestEvent, event, sizeof(SDL_Event));
    uv_rwlock_wrunlock(&ioSDLEventLock);

    uv_async_send(&ioSDLEventAsync);
}

// direct tasks for the event loop

void idleTick(uv_idle_t* handle) {
}

void timerTick(uv_timer_t* handle) {
    printf("tick... %lld\n", uv_now(uv_default_loop()));
}

static int counter = 0;
void uiTick(uv_async_t* handle) {
    uv_rwlock_rdlock(&ioSDLEventLock);
    const int latestType = ioSDLLatestEvent.type;
    uv_rwlock_rdunlock(&ioSDLEventLock);

    printf("something happened from the SDL thread! %d (type %d)\n", counter, latestType);
    if (latestType == SDL_EVENT_QUIT || counter++ > 100) {
        ioRequestQuit();
    }
}

int ioThreadLoop(void *data)
{
    uv_timer_t timer;
    uv_timer_init(uv_default_loop(), &timer);
    uv_unref((uv_handle_t*) &timer); // detach, so it doesn't block exit
    uv_timer_start(&timer, timerTick, 1000, 1000);

    uv_rwlock_init(&ioQuittingLock);
    uv_rwlock_init(&ioSDLEventLock);
    uv_async_init(uv_default_loop(), &ioSDLEventAsync, &uiTick);

    printf("Starting...\n");

    uv_run(uv_default_loop(), UV_RUN_DEFAULT);
    uv_loop_close(uv_default_loop());

    uv_rwlock_destroy(&ioSDLEventLock);
    uv_rwlock_destroy(&ioQuittingLock);

    printf("Bye...\n");
    
    return 1;
}

/* This function runs once at startup. */
SDL_AppResult SDL_AppInit(void **appstate, int argc, char *argv[])
{
    /* Create the window */
    const int flags = SDL_WINDOW_HIGH_PIXEL_DENSITY | SDL_WINDOW_RESIZABLE;
    if (!SDL_CreateWindowAndRenderer("gigi", 800, 600, flags, &window, &renderer)) {
        SDL_Log("Couldn't create window and renderer: %s", SDL_GetError());
        return SDL_APP_FAILURE;
    }
    
    /* Startup the UV EventLoop */
    iothread = SDL_CreateThread(&ioThreadLoop, "uvEventLoop", NULL);
    
    return SDL_APP_CONTINUE;
}

/* This function runs when a new event (mouse input, keypresses, etc) occurs. */
SDL_AppResult SDL_AppEvent(void *appstate, SDL_Event *event)
{
    ioPushSDLEvent(event);
    return SDL_APP_CONTINUE;
}

/* This function runs once per frame, and is the heart of the program. */
SDL_AppResult SDL_AppIterate(void *appstate)
{
    if (ioShouldQuit())
        return SDL_APP_SUCCESS;

    const char *message = RUNTIME_PATH;
    int w = 0, h = 0;
    float x, y;
    const float scale = 1.0f;

    /* Center the message and scale it up */
    SDL_GetRenderOutputSize(renderer, &w, &h);
    SDL_SetRenderScale(renderer, scale, scale);
    x = ((w / scale) - SDL_DEBUG_TEXT_FONT_CHARACTER_SIZE * SDL_strlen(message)) / 2;
    y = ((h / scale) - SDL_DEBUG_TEXT_FONT_CHARACTER_SIZE) / 2;

    const double now = ((double)SDL_GetTicks()) / 1000.0;  /* convert from milliseconds to seconds. */
    /* choose the color for the frame we will draw. The sine wave trick makes it fade between colors smoothly. */
    const float red = (float) (0.5 + 0.5 * SDL_sin(now));
    const float green = (float) (0.5 + 0.5 * SDL_sin(now + SDL_PI_D * 2 / 3));
    const float blue = (float) (0.5 + 0.5 * SDL_sin(now + SDL_PI_D * 4 / 3));
    SDL_SetRenderDrawColorFloat(renderer, red, green, blue, SDL_ALPHA_OPAQUE_FLOAT);  /* new color, full alpha. */
    SDL_RenderClear(renderer);

    /* Draw the message */
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
    SDL_RenderDebugText(renderer, x, y, message);
    SDL_RenderPresent(renderer);

    return SDL_APP_CONTINUE;
}

/* This function runs once at shutdown. */
void SDL_AppQuit(void *appstate, SDL_AppResult result)
{
    uv_stop(uv_default_loop());
    printf("waiting on thread...\n");
    SDL_WaitThread(iothread, NULL);
    printf("bye\n");
}
