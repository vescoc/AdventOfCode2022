use std::marker::PhantomData;
use std::fmt::Display;
use web_sys::HtmlInputElement;
use yew::prelude::*;

#[derive(Properties)]
pub struct ModelProps<SOLVE1, SOLVE2> {
    pub input: String,
    pub solve_1: SOLVE1,
    pub solve_2: SOLVE2,
}

pub enum Msg {
    Run(String),
}

pub struct Model<SOLVE1, P1T, SOLVE2, P2T> {
    input_ref: NodeRef,
    part1: Option<P1T>,
    part2: Option<P2T>,
    input: String,
    _solve1: PhantomData<SOLVE1>,
    _solve2: PhantomData<SOLVE2>,
}

impl<SOLVE1, SOLVE2> PartialEq for ModelProps<SOLVE1, SOLVE2> {
    fn eq(&self, other: &Self) -> bool {
        self.input.eq(&other.input)
    }
}

impl<SOLVE1, P1T, SOLVE2, P2T> Component for Model<SOLVE1, P1T, SOLVE2, P2T>
where SOLVE1: Fn(&str) -> P1T + 'static,
      SOLVE2: Fn(&str) -> P2T + 'static,
      P1T: Display + Copy + 'static,
      P2T: Display + Copy + 'static,
{
    type Message = Msg;
    type Properties = ModelProps<SOLVE1, SOLVE2>;

    fn create(ctx: &Context<Self>) -> Self {
        let input = ctx.props().input.clone();
        
        Self {
            input_ref: NodeRef::default(),
            part1: None,
            part2: None,
            input,
            _solve1: PhantomData,
            _solve2: PhantomData,
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Run(input) => {
                self.part1 = Some((ctx.props().solve_1)(&input));
                self.part2 = Some((ctx.props().solve_2)(&input));
                self.input = input;
                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link();
        let input_ref = self.input_ref.clone();

        let onclick = link.batch_callback(move |_| {
            let input = input_ref.cast::<HtmlInputElement>();
            input.map(|input| Msg::Run(input.value()))
        });
        
        html! {
            <>
                <label for="input"> { "Input: " }
            <textarea id="input" ref={self.input_ref.clone()} rows="4" cols="50" value={self.input.clone()} />
                </label>
                <button {onclick}>{ "\u{23F5}" }</button>
                <label for="results"> { "Results: " }
            <div id="results" class="output">
                <div class="result"><label> { "Part 1: " } </label> { self.part1 }</div>
                <div class="result"><label> { "Part 2: " } </label> { self.part2 }</div>
            </div>
            </label>
                </>
        }
    }
}